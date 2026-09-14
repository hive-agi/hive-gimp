//! The Rust half of hive-gimp's native GIMP 3 plug-in.
//!
//! A cdylib whose `cljrs_init` interns `gimp.native/*` into a clojurust
//! environment. It supplies the three things clojurust cannot do in Clojure:
//!
//!   1. THE PLUG-IN ENTRY. `start` registers a GimpPlugIn GObject subclass from
//!      raw FFI (class_init writes the query_procedures and create_procedure
//!      vfuncs) and runs libgimp's `gimp_main` on a thread of its own.
//!   2. THE SOCKET. `listen`, `next-request` and `respond` accept on 127.0.0.1
//!      and frame one JSON object per connection.
//!   3. THE GIMP PORT. Thin wrappers over libgimp C calls, taking and returning
//!      GIMP integer object ids and scalars.
//!
//! What a command MEANS is decided in Clojure (hive-gimp.plugin.dispatch).
//!
//! ## No Clojure runs inside a callback from this library
//!
//! The obvious design has libgimp call the procedure's run function, and the
//! run function call back into a Clojure handler. It does not survive contact:
//! a Clojure fn invoked from here through cljrs_runtime::env::callback::invoke
//! panics in rpds ("cannot have a branch at this height") the first time a
//! vector it builds outgrows one 32-slot leaf, while the same fn called
//! directly from cljrs is fine. This cdylib statically links its own copy of
//! the clojurust runtime crates, and a callback runs THAT copy over values the
//! host binary built. So control stays with the host interpreter:
//!
//!   cljrs main thread                     gimp thread (spawned here)
//!   (start argv proc) ------------------> gimp_main
//!   (await-phase ms)  <-- "running" ----- run_procedure: signal, then WAIT
//!   loop: next-request / dispatch
//!         (libgimp calls, this thread)
//!         / respond
//!   (finish)          ------------------> run_procedure returns SUCCESS
//!                     <-- exit code ----- gimp_main returns
//!
//! libgimp's PDB calls are made from the cljrs thread while the gimp thread is
//! parked on a condvar, so exactly one thread talks to GIMP at a time. The
//! same rule keeps every value crossing the boundary a scalar: strings, longs,
//! doubles, booleans, nil. Id lists cross as comma-separated strings.
//!
//! Nothing GIMP is linked at build time. libgimp-3.0, libgimpbase, GLib,
//! GObject, GIO and GEGL are dlopened by soname when the plug-in runs inside
//! GIMP's flatpak sandbox. Signatures are GIMP 3.2's, checked against the
//! flatpak's /app/include/gimp-3.0 and declared by hand.

use std::ffi::{CStr, CString, c_void};
use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::os::raw::{c_char, c_int, c_uint};
use std::sync::{Condvar, Mutex, OnceLock};
use std::time::{Duration, Instant};

use cljrs_interop::{FromValue, Registry, wrap_fn0, wrap_fn1, wrap_fn2, wrap_fn3, wrap_fn_variadic};
use cljrs_value::Value;

type Ptr = *mut c_void;
type GType = usize;
type GBool = c_int;

// Enum values from libgimpbase/gimpbaseenums.h and libgimp/gimpenums.h (3.2).
const GIMP_PDB_PROC_TYPE_PLUGIN: c_int = 1;
const GIMP_PDB_SUCCESS: c_int = 3;
const GIMP_RUN_NONINTERACTIVE: c_int = 1;
const GIMP_FILL_BACKGROUND: c_int = 1;
const GIMP_FILL_TRANSPARENT: c_int = 4;
const GIMP_LAYER_MODE_NORMAL: c_int = 28;
const G_PARAM_READWRITE: c_uint = 3;

/// sizeof(GObjectClass) on LP64: GTypeClass, construct_properties, seven
/// method pointers, constructed, flags, n_construct_properties, pspecs,
/// n_pspecs, pdummy[3]. The GimpPlugInClass vfuncs follow it in order:
/// query_procedures, init_procedures, create_procedure, quit, set_i18n.
const GOBJECT_CLASS_SIZE: usize = 136;
const VFUNC_QUERY_PROCEDURES: usize = GOBJECT_CLASS_SIZE;
const VFUNC_CREATE_PROCEDURE: usize = GOBJECT_CLASS_SIZE + 2 * 8;
const VFUNC_SET_I18N: usize = GOBJECT_CLASS_SIZE + 4 * 8;

#[repr(C)]
struct GTypeQuery {
    type_: GType,
    type_name: *const c_char,
    class_size: c_uint,
    instance_size: c_uint,
}

type RunFunc = unsafe extern "C" fn(Ptr, Ptr, Ptr) -> Ptr;
type ClassInit = unsafe extern "C" fn(Ptr, Ptr);

struct Gimp {
    _libs: Vec<libloading::Library>,
    g_strdup: unsafe extern "C" fn(*const c_char) -> *mut c_char,
    g_free: unsafe extern "C" fn(Ptr),
    g_list_append: unsafe extern "C" fn(Ptr, Ptr) -> Ptr,
    g_type_query: unsafe extern "C" fn(GType, *mut GTypeQuery),
    g_type_register_static_simple:
        unsafe extern "C" fn(GType, *const c_char, c_uint, ClassInit, c_uint, Ptr, c_uint) -> GType,
    g_object_unref: unsafe extern "C" fn(Ptr),
    g_file_new_for_path: unsafe extern "C" fn(*const c_char) -> Ptr,
    g_file_get_path: unsafe extern "C" fn(Ptr) -> *mut c_char,
    gegl_color_new: unsafe extern "C" fn(*const c_char) -> Ptr,
    gimp_run_mode_get_type: unsafe extern "C" fn() -> GType,
    gimp_plug_in_get_type: unsafe extern "C" fn() -> GType,
    gimp_main: unsafe extern "C" fn(GType, c_int, *mut *mut c_char) -> c_int,
    gimp_procedure_new: unsafe extern "C" fn(Ptr, *const c_char, c_int, RunFunc, Ptr, Ptr) -> Ptr,
    gimp_procedure_add_enum_argument:
        unsafe extern "C" fn(Ptr, *const c_char, *const c_char, *const c_char, GType, c_int, c_uint),
    gimp_procedure_set_documentation: unsafe extern "C" fn(Ptr, *const c_char, *const c_char, *const c_char),
    gimp_procedure_new_return_values: unsafe extern "C" fn(Ptr, c_int, Ptr) -> Ptr,
    gimp_version: unsafe extern "C" fn() -> *mut c_char,
    gimp_get_images: unsafe extern "C" fn() -> *mut Ptr,
    gimp_image_get_id: unsafe extern "C" fn(Ptr) -> i32,
    gimp_image_get_by_id: unsafe extern "C" fn(i32) -> Ptr,
    gimp_image_get_width: unsafe extern "C" fn(Ptr) -> c_int,
    gimp_image_get_height: unsafe extern "C" fn(Ptr) -> c_int,
    gimp_image_get_base_type: unsafe extern "C" fn(Ptr) -> c_int,
    gimp_image_get_layers: unsafe extern "C" fn(Ptr) -> *mut Ptr,
    gimp_image_get_file: unsafe extern "C" fn(Ptr) -> Ptr,
    gimp_image_new: unsafe extern "C" fn(c_int, c_int, c_int) -> Ptr,
    gimp_image_duplicate: unsafe extern "C" fn(Ptr) -> Ptr,
    gimp_image_flatten: unsafe extern "C" fn(Ptr) -> Ptr,
    gimp_image_delete: unsafe extern "C" fn(Ptr) -> GBool,
    gimp_image_insert_layer: unsafe extern "C" fn(Ptr, Ptr, Ptr, c_int) -> GBool,
    gimp_layer_new: unsafe extern "C" fn(Ptr, *const c_char, c_int, c_int, c_int, f64, c_int) -> Ptr,
    gimp_layer_get_opacity: unsafe extern "C" fn(Ptr) -> f64,
    gimp_layer_add_alpha: unsafe extern "C" fn(Ptr) -> GBool,
    gimp_item_get_id: unsafe extern "C" fn(Ptr) -> i32,
    gimp_item_get_by_id: unsafe extern "C" fn(i32) -> Ptr,
    gimp_item_get_name: unsafe extern "C" fn(Ptr) -> *mut c_char,
    gimp_item_get_visible: unsafe extern "C" fn(Ptr) -> GBool,
    gimp_drawable_get_width: unsafe extern "C" fn(Ptr) -> c_int,
    gimp_drawable_get_height: unsafe extern "C" fn(Ptr) -> c_int,
    gimp_drawable_has_alpha: unsafe extern "C" fn(Ptr) -> GBool,
    gimp_drawable_fill: unsafe extern "C" fn(Ptr, c_int) -> GBool,
    gimp_context_set_background: unsafe extern "C" fn(Ptr) -> GBool,
    gimp_display_new: unsafe extern "C" fn(Ptr) -> Ptr,
    gimp_displays_flush: unsafe extern "C" fn() -> GBool,
    gimp_file_save: unsafe extern "C" fn(c_int, Ptr, Ptr, Ptr) -> GBool,
}

unsafe impl Send for Gimp {}
unsafe impl Sync for Gimp {}

static GIMP: OnceLock<Result<Gimp, String>> = OnceLock::new();

const SONAMES: [&str; 6] = [
    "libglib-2.0.so.0",
    "libgobject-2.0.so.0",
    "libgio-2.0.so.0",
    "libgegl-0.4.so.0",
    "libgimpbase-3.0.so.0",
    "libgimp-3.0.so.0",
];

/// The first library in LIBS that exports NAME, as a function pointer.
macro_rules! sym {
    ($libs:expr, $name:literal) => {{
        let key = concat!($name, "\0").as_bytes();
        let mut found = None;
        for lib in $libs.iter() {
            if let Ok(s) = unsafe { lib.get(key) } {
                found = Some(*s);
                break;
            }
        }
        found.ok_or_else(|| format!("symbol {} not found in {:?}", $name, SONAMES))?
    }};
}

fn open() -> Result<Gimp, String> {
    let mut libs = Vec::new();
    for soname in SONAMES {
        let lib = unsafe { libloading::Library::new(soname) }
            .map_err(|e| format!("{soname}: {e} (the native plug-in must run inside GIMP's environment)"))?;
        libs.push(lib);
    }
    Ok(Gimp {
        g_strdup: sym!(libs, "g_strdup"),
        g_free: sym!(libs, "g_free"),
        g_list_append: sym!(libs, "g_list_append"),
        g_type_query: sym!(libs, "g_type_query"),
        g_type_register_static_simple: sym!(libs, "g_type_register_static_simple"),
        g_object_unref: sym!(libs, "g_object_unref"),
        g_file_new_for_path: sym!(libs, "g_file_new_for_path"),
        g_file_get_path: sym!(libs, "g_file_get_path"),
        gegl_color_new: sym!(libs, "gegl_color_new"),
        gimp_run_mode_get_type: sym!(libs, "gimp_run_mode_get_type"),
        gimp_plug_in_get_type: sym!(libs, "gimp_plug_in_get_type"),
        gimp_main: sym!(libs, "gimp_main"),
        gimp_procedure_new: sym!(libs, "gimp_procedure_new"),
        gimp_procedure_add_enum_argument: sym!(libs, "gimp_procedure_add_enum_argument"),
        gimp_procedure_set_documentation: sym!(libs, "gimp_procedure_set_documentation"),
        gimp_procedure_new_return_values: sym!(libs, "gimp_procedure_new_return_values"),
        gimp_version: sym!(libs, "gimp_version"),
        gimp_get_images: sym!(libs, "gimp_get_images"),
        gimp_image_get_id: sym!(libs, "gimp_image_get_id"),
        gimp_image_get_by_id: sym!(libs, "gimp_image_get_by_id"),
        gimp_image_get_width: sym!(libs, "gimp_image_get_width"),
        gimp_image_get_height: sym!(libs, "gimp_image_get_height"),
        gimp_image_get_base_type: sym!(libs, "gimp_image_get_base_type"),
        gimp_image_get_layers: sym!(libs, "gimp_image_get_layers"),
        gimp_image_get_file: sym!(libs, "gimp_image_get_file"),
        gimp_image_new: sym!(libs, "gimp_image_new"),
        gimp_image_duplicate: sym!(libs, "gimp_image_duplicate"),
        gimp_image_flatten: sym!(libs, "gimp_image_flatten"),
        gimp_image_delete: sym!(libs, "gimp_image_delete"),
        gimp_image_insert_layer: sym!(libs, "gimp_image_insert_layer"),
        gimp_layer_new: sym!(libs, "gimp_layer_new"),
        gimp_layer_get_opacity: sym!(libs, "gimp_layer_get_opacity"),
        gimp_layer_add_alpha: sym!(libs, "gimp_layer_add_alpha"),
        gimp_item_get_id: sym!(libs, "gimp_item_get_id"),
        gimp_item_get_by_id: sym!(libs, "gimp_item_get_by_id"),
        gimp_item_get_name: sym!(libs, "gimp_item_get_name"),
        gimp_item_get_visible: sym!(libs, "gimp_item_get_visible"),
        gimp_drawable_get_width: sym!(libs, "gimp_drawable_get_width"),
        gimp_drawable_get_height: sym!(libs, "gimp_drawable_get_height"),
        gimp_drawable_has_alpha: sym!(libs, "gimp_drawable_has_alpha"),
        gimp_drawable_fill: sym!(libs, "gimp_drawable_fill"),
        gimp_context_set_background: sym!(libs, "gimp_context_set_background"),
        gimp_display_new: sym!(libs, "gimp_display_new"),
        gimp_displays_flush: sym!(libs, "gimp_displays_flush"),
        gimp_file_save: sym!(libs, "gimp_file_save"),
        _libs: libs,
    })
}

fn gimp() -> Result<&'static Gimp, String> {
    GIMP.get_or_init(open).as_ref().map_err(|e| e.clone())
}

// ---------------------------------------------------------------------------
// The plug-in lifecycle: gimp_main on its own thread, parked while cljrs works

#[derive(Clone, Debug, PartialEq)]
enum Phase {
    Idle,
    Started,
    Running,
    Finishing,
    Exited(i64),
}

struct Life {
    phase: Mutex<Phase>,
    changed: Condvar,
}

static LIFE: Life = Life { phase: Mutex::new(Phase::Idle), changed: Condvar::new() };
static PROCEDURE: OnceLock<CString> = OnceLock::new();

fn set_phase(p: Phase) {
    *LIFE.phase.lock().unwrap_or_else(|e| e.into_inner()) = p;
    LIFE.changed.notify_all();
}

fn wait_phase(until: impl Fn(&Phase) -> bool, timeout: Duration) -> Phase {
    let deadline = Instant::now() + timeout;
    let mut phase = LIFE.phase.lock().unwrap_or_else(|e| e.into_inner());
    while !until(&phase) {
        let left = deadline.saturating_duration_since(Instant::now());
        if left.is_zero() {
            break;
        }
        phase = LIFE.changed.wait_timeout(phase, left).unwrap_or_else(|e| e.into_inner()).0;
    }
    phase.clone()
}

unsafe extern "C" fn query_procedures(_plug_in: Ptr) -> Ptr {
    let (Ok(g), Some(name)) = (gimp(), PROCEDURE.get()) else { return std::ptr::null_mut() };
    unsafe { (g.g_list_append)(std::ptr::null_mut(), (g.g_strdup)(name.as_ptr()) as Ptr) }
}

unsafe extern "C" fn set_i18n(_: Ptr, _: *const c_char, _: *mut *mut c_char, _: *mut *mut c_char) -> GBool {
    0
}

/// The procedure's run, on the gimp thread: announce, then park until cljrs
/// has finished serving. No Clojure is called from here (see the module doc).
unsafe extern "C" fn run_procedure(procedure: Ptr, _config: Ptr, _run_data: Ptr) -> Ptr {
    let Ok(g) = gimp() else { return std::ptr::null_mut() };
    set_phase(Phase::Running);
    wait_phase(|p| *p == Phase::Finishing, Duration::from_secs(u64::MAX / 4));
    unsafe { (g.gimp_procedure_new_return_values)(procedure, GIMP_PDB_SUCCESS, std::ptr::null_mut()) }
}

unsafe extern "C" fn create_procedure(plug_in: Ptr, name: *const c_char) -> Ptr {
    let (Ok(g), Some(ours)) = (gimp(), PROCEDURE.get()) else { return std::ptr::null_mut() };
    if unsafe { CStr::from_ptr(name) } != ours.as_c_str() {
        return std::ptr::null_mut();
    }
    unsafe {
        let procedure = (g.gimp_procedure_new)(
            plug_in,
            name,
            GIMP_PDB_PROC_TYPE_PLUGIN,
            run_procedure,
            std::ptr::null_mut(),
            std::ptr::null_mut(),
        );
        (g.gimp_procedure_add_enum_argument)(
            procedure,
            c"run-mode".as_ptr(),
            c"Run mode".as_ptr(),
            c"The run mode".as_ptr(),
            (g.gimp_run_mode_get_type)(),
            GIMP_RUN_NONINTERACTIVE,
            G_PARAM_READWRITE,
        );
        (g.gimp_procedure_set_documentation)(
            procedure,
            c"Serve hive-gimp's socket protocol from a clojurust plug-in".as_ptr(),
            c"Blocks, answering one JSON command per connection on 127.0.0.1, until a client sends quit_server.".as_ptr(),
            std::ptr::null(),
        );
        procedure
    }
}

unsafe extern "C" fn class_init(klass: Ptr, _data: Ptr) {
    let base = klass as *mut Ptr;
    unsafe {
        *base.byte_add(VFUNC_QUERY_PROCEDURES) = query_procedures as Ptr;
        *base.byte_add(VFUNC_CREATE_PROCEDURE) = create_procedure as Ptr;
        *base.byte_add(VFUNC_SET_I18N) = set_i18n as Ptr;
    }
}

/// (start argv-lines procedure-name) -> true. ARGV is newline-separated, so it
/// crosses as one string. gimp_main runs on a new thread.
fn start(argv: String, procedure: String) -> Result<bool, String> {
    let g = gimp()?;
    PROCEDURE.set(CString::new(procedure).map_err(|e| e.to_string())?).map_err(|_| "already started".to_string())?;
    let args: Vec<CString> = argv
        .split('\n')
        .map(|a| CString::new(a).map_err(|e| e.to_string()))
        .collect::<Result<_, _>>()?;
    let ty = unsafe {
        let parent = (g.gimp_plug_in_get_type)();
        let mut q = GTypeQuery { type_: 0, type_name: std::ptr::null(), class_size: 0, instance_size: 0 };
        (g.g_type_query)(parent, &mut q);
        if (q.class_size as usize) < VFUNC_SET_I18N + 8 {
            return Err(format!("GimpPlugInClass is {} bytes, smaller than the vfunc layout assumed", q.class_size));
        }
        (g.g_type_register_static_simple)(
            parent,
            c"HiveGimpNativePlugIn".as_ptr(),
            q.class_size,
            class_init,
            q.instance_size,
            std::ptr::null_mut(),
            0,
        )
    };
    set_phase(Phase::Started);
    std::thread::Builder::new()
        .name("gimp-main".into())
        .spawn(move || {
            let mut raw: Vec<*mut c_char> = args.iter().map(|a| a.as_ptr() as *mut c_char).collect();
            raw.push(std::ptr::null_mut());
            let code = unsafe { (g.gimp_main)(ty, args.len() as c_int, raw.as_mut_ptr()) };
            set_phase(Phase::Exited(code as i64));
        })
        .map_err(|e| e.to_string())?;
    Ok(true)
}

fn phase_name(p: &Phase) -> String {
    match p {
        Phase::Idle => "idle".into(),
        Phase::Started => "started".into(),
        Phase::Running => "running".into(),
        Phase::Finishing => "finishing".into(),
        Phase::Exited(code) => format!("exited:{code}"),
    }
}

// ---------------------------------------------------------------------------
// The socket: one connection, one JSON object, one answer

static LISTENER: Mutex<Option<TcpListener>> = Mutex::new(None);
static CURRENT: Mutex<Option<TcpStream>> = Mutex::new(None);

/// Read one JSON object from STREAM. When the peer half-closes before the
/// bytes form an object, what did arrive is still returned, so the Clojure side
/// answers "unparseable" instead of the client reading silence. None only
/// when nothing arrived at all.
fn read_request(stream: &mut TcpStream) -> Option<String> {
    let mut buf = Vec::new();
    let mut chunk = [0u8; 8192];
    loop {
        match stream.read(&mut chunk) {
            Ok(0) | Err(_) => {
                return if buf.is_empty() { None } else { Some(String::from_utf8_lossy(&buf).into_owned()) };
            }
            Ok(n) => {
                buf.extend_from_slice(&chunk[..n]);
                if let Ok(serde_json::Value::Object(_)) = serde_json::from_slice::<serde_json::Value>(&buf) {
                    return String::from_utf8(buf).ok();
                }
            }
        }
    }
}

fn listen(port: i64) -> Result<String, String> {
    let l = TcpListener::bind(("127.0.0.1", port as u16)).map_err(|e| format!("bind 127.0.0.1:{port}: {e}"))?;
    let addr = l.local_addr().map(|a| a.to_string()).unwrap_or_default();
    *LISTENER.lock().unwrap_or_else(|e| e.into_inner()) = Some(l);
    Ok(addr)
}

/// Block until a client sends one complete request; its text. A connection
/// that closes before a whole object arrives is dropped and the next awaited.
fn next_request() -> Result<String, String> {
    let guard = LISTENER.lock().unwrap_or_else(|e| e.into_inner());
    let listener = guard.as_ref().ok_or("listen has not been called")?;
    loop {
        let (mut stream, _) = listener.accept().map_err(|e| e.to_string())?;
        let _ = stream.set_read_timeout(Some(Duration::from_secs(30)));
        if let Some(text) = read_request(&mut stream) {
            *CURRENT.lock().unwrap_or_else(|e| e.into_inner()) = Some(stream);
            return Ok(text);
        }
    }
}

fn respond(text: String) -> Result<bool, String> {
    let mut stream = CURRENT.lock().unwrap_or_else(|e| e.into_inner()).take().ok_or("no request is awaiting an answer")?;
    stream.write_all(text.as_bytes()).map_err(|e| e.to_string())?;
    let _ = stream.flush();
    Ok(true)
}

// ---------------------------------------------------------------------------
// The GIMP port

fn take_string(g: &Gimp, p: *mut c_char) -> Option<String> {
    if p.is_null() {
        return None;
    }
    let s = unsafe { CStr::from_ptr(p) }.to_string_lossy().into_owned();
    unsafe { (g.g_free)(p as Ptr) };
    Some(s)
}

/// Comma-separated ids of a NULL-terminated, transfer-container object array.
fn take_ids(g: &Gimp, arr: *mut Ptr, id_of: unsafe extern "C" fn(Ptr) -> i32) -> String {
    let mut out = Vec::new();
    if arr.is_null() {
        return String::new();
    }
    let mut i = 0;
    loop {
        let p = unsafe { *arr.add(i) };
        if p.is_null() {
            break;
        }
        out.push(unsafe { id_of(p) }.to_string());
        i += 1;
    }
    unsafe { (g.g_free)(arr as Ptr) };
    out.join(",")
}

fn image(g: &Gimp, id: i64) -> Result<Ptr, String> {
    let p = unsafe { (g.gimp_image_get_by_id)(id as i32) };
    if p.is_null() { Err(format!("no image with id {id}")) } else { Ok(p) }
}

fn item(g: &Gimp, id: i64) -> Result<Ptr, String> {
    let p = unsafe { (g.gimp_item_get_by_id)(id as i32) };
    if p.is_null() { Err(format!("no layer or drawable with id {id}")) } else { Ok(p) }
}

fn cstring(s: &str) -> Result<CString, String> {
    CString::new(s).map_err(|e| format!("argument holds a NUL byte: {e}"))
}

fn long_arg(args: &[Value], i: usize) -> Result<i64, String> {
    args.get(i).ok_or(format!("argument {i} missing")).and_then(|v| i64::from_value(v).map_err(|e| e.to_string()))
}

fn f64_arg(args: &[Value], i: usize) -> Result<f64, String> {
    match args.get(i) {
        Some(Value::Long(n)) => Ok(*n as f64),
        Some(v) => f64::from_value(v).map_err(|e| e.to_string()),
        None => Err(format!("argument {i} missing")),
    }
}

fn string_arg(args: &[Value], i: usize) -> Result<String, String> {
    args.get(i).ok_or(format!("argument {i} missing")).and_then(|v| String::from_value(v).map_err(|e| e.to_string()))
}

/// Register `gimp.native/*`.
///
/// # Safety
/// `registry` must be a valid, non-null pointer to a `Registry` that outlives
/// this call. The cljrs loader guarantees both.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cljrs_init(registry: *mut Registry) {
    if registry.is_null() {
        eprintln!("gimp.native: cljrs_init received a NULL registry");
        return;
    }
    let reg = unsafe { &mut *registry };
    let ns = "gimp.native";

    reg.define_in(ns, "available?", wrap_fn0("available?", || Ok::<bool, String>(gimp().is_ok())));
    reg.define_in(ns, "diagnose", wrap_fn0("diagnose", || {
        Ok::<String, String>(match gimp() {
            Ok(_) => "libgimp loaded".into(),
            Err(e) => e,
        })
    }));

    // Lifecycle.
    reg.define_in(ns, "start", wrap_fn2("start", |argv: String, procedure: String| start(argv, procedure)));
    reg.define_in(ns, "await-phase", wrap_fn1("await-phase", |ms: i64| {
        let p = wait_phase(|p| matches!(p, Phase::Running | Phase::Exited(_)), Duration::from_millis(ms.max(0) as u64));
        Ok::<String, String>(phase_name(&p))
    }));
    reg.define_in(ns, "finish", wrap_fn1("finish", |ms: i64| {
        set_phase(Phase::Finishing);
        let p = wait_phase(|p| matches!(p, Phase::Exited(_)), Duration::from_millis(ms.max(0) as u64));
        Ok::<String, String>(phase_name(&p))
    }));

    // Socket.
    reg.define_in(ns, "listen", wrap_fn1("listen", |port: i64| listen(port)));
    reg.define_in(ns, "next-request", wrap_fn0("next-request", next_request));
    reg.define_in(ns, "respond", wrap_fn1("respond", |text: String| respond(text)));

    // GIMP port.
    reg.define_in(ns, "version", wrap_fn0("version", || {
        let g = gimp()?;
        take_string(g, unsafe { (g.gimp_version)() }).ok_or_else(|| "gimp_version returned NULL".to_string())
    }));
    reg.define_in(ns, "image-ids", wrap_fn0("image-ids", || {
        let g = gimp()?;
        Ok::<String, String>(take_ids(g, unsafe { (g.gimp_get_images)() }, g.gimp_image_get_id))
    }));
    reg.define_in(ns, "image-width", wrap_fn1("image-width", |id: i64| {
        let g = gimp()?;
        Ok::<i64, String>(unsafe { (g.gimp_image_get_width)(image(g, id)?) } as i64)
    }));
    reg.define_in(ns, "image-height", wrap_fn1("image-height", |id: i64| {
        let g = gimp()?;
        Ok::<i64, String>(unsafe { (g.gimp_image_get_height)(image(g, id)?) } as i64)
    }));
    reg.define_in(ns, "image-base-type", wrap_fn1("image-base-type", |id: i64| {
        let g = gimp()?;
        Ok::<i64, String>(unsafe { (g.gimp_image_get_base_type)(image(g, id)?) } as i64)
    }));
    reg.define_in(ns, "image-layer-ids", wrap_fn1("image-layer-ids", |id: i64| {
        let g = gimp()?;
        let img = image(g, id)?;
        Ok::<String, String>(take_ids(g, unsafe { (g.gimp_image_get_layers)(img) }, g.gimp_item_get_id))
    }));
    reg.define_in(ns, "image-file", wrap_fn1("image-file", |id: i64| {
        let g = gimp()?;
        let file = unsafe { (g.gimp_image_get_file)(image(g, id)?) };
        if file.is_null() {
            return Ok::<Option<String>, String>(None);
        }
        let path = take_string(g, unsafe { (g.g_file_get_path)(file) });
        unsafe { (g.g_object_unref)(file) };
        Ok(path)
    }));
    reg.define_in(ns, "image-new", wrap_fn3("image-new", |w: i64, h: i64, base: i64| {
        let g = gimp()?;
        let img = unsafe { (g.gimp_image_new)(w as c_int, h as c_int, base as c_int) };
        if img.is_null() {
            return Err("gimp_image_new returned NULL".to_string());
        }
        Ok::<i64, String>(unsafe { (g.gimp_image_get_id)(img) } as i64)
    }));
    reg.define_in(ns, "image-duplicate", wrap_fn1("image-duplicate", |id: i64| {
        let g = gimp()?;
        let dup = unsafe { (g.gimp_image_duplicate)(image(g, id)?) };
        if dup.is_null() {
            return Err("gimp_image_duplicate returned NULL".to_string());
        }
        Ok::<i64, String>(unsafe { (g.gimp_image_get_id)(dup) } as i64)
    }));
    reg.define_in(ns, "image-flatten", wrap_fn1("image-flatten", |id: i64| {
        let g = gimp()?;
        Ok::<bool, String>(!unsafe { (g.gimp_image_flatten)(image(g, id)?) }.is_null())
    }));
    reg.define_in(ns, "image-delete", wrap_fn1("image-delete", |id: i64| {
        let g = gimp()?;
        Ok::<bool, String>(unsafe { (g.gimp_image_delete)(image(g, id)?) } != 0)
    }));
    // (layer-new image name width height image-type opacity) -> layer id, not inserted.
    reg.define_in(ns, "layer-new", wrap_fn_variadic("layer-new", 6, |args: &[Value]| {
        let g = gimp()?;
        let img = image(g, long_arg(args, 0)?)?;
        let name = cstring(&string_arg(args, 1)?)?;
        let layer = unsafe {
            (g.gimp_layer_new)(
                img,
                name.as_ptr(),
                long_arg(args, 2)? as c_int,
                long_arg(args, 3)? as c_int,
                long_arg(args, 4)? as c_int,
                f64_arg(args, 5)?,
                GIMP_LAYER_MODE_NORMAL,
            )
        };
        if layer.is_null() {
            return Err("gimp_layer_new returned NULL".to_string());
        }
        Ok::<i64, String>(unsafe { (g.gimp_item_get_id)(layer) } as i64)
    }));
    reg.define_in(ns, "layer-insert", wrap_fn3("layer-insert", |img: i64, layer: i64, position: i64| {
        let g = gimp()?;
        let ok = unsafe {
            (g.gimp_image_insert_layer)(image(g, img)?, item(g, layer)?, std::ptr::null_mut(), position as c_int)
        };
        if ok == 0 { Err(format!("gimp_image_insert_layer refused layer {layer}")) } else { Ok::<bool, String>(true) }
    }));
    reg.define_in(ns, "layer-name", wrap_fn1("layer-name", |id: i64| {
        let g = gimp()?;
        Ok::<Option<String>, String>(take_string(g, unsafe { (g.gimp_item_get_name)(item(g, id)?) }))
    }));
    reg.define_in(ns, "layer-visible?", wrap_fn1("layer-visible?", |id: i64| {
        let g = gimp()?;
        Ok::<bool, String>(unsafe { (g.gimp_item_get_visible)(item(g, id)?) } != 0)
    }));
    reg.define_in(ns, "layer-opacity", wrap_fn1("layer-opacity", |id: i64| {
        let g = gimp()?;
        Ok::<f64, String>(unsafe { (g.gimp_layer_get_opacity)(item(g, id)?) })
    }));
    reg.define_in(ns, "layer-add-alpha", wrap_fn1("layer-add-alpha", |id: i64| {
        let g = gimp()?;
        Ok::<bool, String>(unsafe { (g.gimp_layer_add_alpha)(item(g, id)?) } != 0)
    }));
    reg.define_in(ns, "drawable-width", wrap_fn1("drawable-width", |id: i64| {
        let g = gimp()?;
        Ok::<i64, String>(unsafe { (g.gimp_drawable_get_width)(item(g, id)?) } as i64)
    }));
    reg.define_in(ns, "drawable-height", wrap_fn1("drawable-height", |id: i64| {
        let g = gimp()?;
        Ok::<i64, String>(unsafe { (g.gimp_drawable_get_height)(item(g, id)?) } as i64)
    }));
    reg.define_in(ns, "drawable-has-alpha?", wrap_fn1("drawable-has-alpha?", |id: i64| {
        let g = gimp()?;
        Ok::<bool, String>(unsafe { (g.gimp_drawable_has_alpha)(item(g, id)?) } != 0)
    }));
    // (fill-color drawable colour) -> bool. COLOUR is anything GEGL parses: a
    // CSS name, #rrggbb, rgb(...). Set as the context BACKGROUND and filled
    // with it, the way the reference plug-in fills a new canvas.
    reg.define_in(ns, "fill-color", wrap_fn2("fill-color", |id: i64, colour: String| {
        let g = gimp()?;
        let drawable = item(g, id)?;
        let spec = cstring(&colour)?;
        unsafe {
            let color = (g.gegl_color_new)(spec.as_ptr());
            if color.is_null() {
                return Err(format!("GEGL cannot parse the colour {colour:?}"));
            }
            let set = (g.gimp_context_set_background)(color);
            (g.g_object_unref)(color);
            if set == 0 {
                return Ok::<bool, String>(false);
            }
            Ok((g.gimp_drawable_fill)(drawable, GIMP_FILL_BACKGROUND) != 0)
        }
    }));
    reg.define_in(ns, "fill-transparent", wrap_fn1("fill-transparent", |id: i64| {
        let g = gimp()?;
        Ok::<bool, String>(unsafe { (g.gimp_drawable_fill)(item(g, id)?, GIMP_FILL_TRANSPARENT) } != 0)
    }));
    reg.define_in(ns, "display-new", wrap_fn1("display-new", |id: i64| {
        let g = gimp()?;
        Ok::<bool, String>(!unsafe { (g.gimp_display_new)(image(g, id)?) }.is_null())
    }));
    reg.define_in(ns, "displays-flush", wrap_fn0("displays-flush", || {
        let g = gimp()?;
        Ok::<bool, String>(unsafe { (g.gimp_displays_flush)() } != 0)
    }));
    reg.define_in(ns, "file-save", wrap_fn2("file-save", |id: i64, path: String| {
        let g = gimp()?;
        let img = image(g, id)?;
        let p = cstring(&path)?;
        unsafe {
            let file = (g.g_file_new_for_path)(p.as_ptr());
            let ok = (g.gimp_file_save)(GIMP_RUN_NONINTERACTIVE, img, file, std::ptr::null_mut());
            (g.g_object_unref)(file);
            Ok::<bool, String>(ok != 0)
        }
    }));

    reg.env().mark_loaded(ns);
}
