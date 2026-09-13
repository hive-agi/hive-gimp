#!/usr/bin/env python3
"""Extract the GIMP-MCP wire contract from the referential project into EDN.

Source of truth is gimp_mcp_server.py: every @mcp.tool() is a uniform
    conn.send_command("<type>", { "<param>": <param>, ... })
so the tool surface is DATA, not code. We read it with `ast` (never regex) and
emit one descriptor per tool:

    {:command "auto_levels"
     :tool    "gimp_auto_levels"
     :doc     "..."
     :params  [{:name "image-index" :wire "image_index" :type :long
                :default 0 :optional? true :doc "..."}]
     :returns :map}

The plugin's dispatch table (gimp-mcp-plugin.py) is read too, purely to assert
that every command we emit is actually handled on the GIMP side.
"""
import ast
import json
import re
import sys
from pathlib import Path

REF = Path("/home/leibniz/PP/referential-projects/gimp-mcp")
SERVER = REF / "gimp_mcp_server.py"
PLUGIN = REF / "gimp-mcp-plugin.py"


# ---------------------------------------------------------------- plugin side

def plugin_commands(src: str) -> set:
    """Command `type` strings the GIMP plugin actually dispatches on."""
    return set(re.findall(r'j\["type"\]\s*==\s*"([a-z0-9_]+)"', src))


# ---------------------------------------------------------------- server side

TYPE_MAP = {
    "int": ":long",
    "float": ":double",
    "str": ":string",
    "bool": ":boolean",
    "list": ":vector",
    "dict": ":map",
}


def ann_to_schema(node):
    """Python annotation to a malli-ish schema keyword, plus optionality."""
    if node is None:
        return ":any", False
    # `int | None`
    if isinstance(node, ast.BinOp) and isinstance(node.op, ast.BitOr):
        left, lopt = ann_to_schema(node.left)
        right, ropt = ann_to_schema(node.right)
        if right == ":nil":
            return left, True
        if left == ":nil":
            return right, True
        return ":any", lopt or ropt
    if isinstance(node, ast.Constant) and node.value is None:
        return ":nil", True
    if isinstance(node, ast.Name):
        return TYPE_MAP.get(node.id, ":any"), False
    if isinstance(node, ast.Subscript):  # list[int] etc.
        return ann_to_schema(node.value)
    return ":any", False


def literal(node):
    """Default value as (value, present?)."""
    if node is None:
        return None, False
    try:
        v = ast.literal_eval(node)
    except Exception:
        return None, False
    return v, True


def kebab(s: str) -> str:
    return s.replace("_", "-")


PARAM_DOC = re.compile(r"^\s*-\s*([a-z0-9_]+)\s*:\s*(.+)$", re.I)


def parse_doc(doc: str):
    """(summary, {param: description}) from the uniform docstring shape."""
    if not doc:
        return "", {}
    lines = doc.strip().splitlines()
    summary = lines[0].strip() if lines else ""
    params = {}
    for line in lines[1:]:
        m = PARAM_DOC.match(line)
        if m:
            params[m.group(1)] = m.group(2).strip()
    return summary, params


def wire_command(fn: ast.FunctionDef):
    """The command `type` this tool sends, or None if it is not a bridge tool."""
    for node in ast.walk(fn):
        if (isinstance(node, ast.Call)
                and isinstance(node.func, ast.Attribute)
                and node.func.attr == "send_command"
                and node.args
                and isinstance(node.args[0], ast.Constant)
                and isinstance(node.args[0].value, str)):
            return node.args[0].value
    return None


def is_mcp_tool(fn: ast.FunctionDef) -> bool:
    for d in fn.decorator_list:
        f = d.func if isinstance(d, ast.Call) else d
        if isinstance(f, ast.Attribute) and f.attr in ("tool", "prompt"):
            return f.attr == "tool"
    return False


def extract(fn: ast.FunctionDef):
    cmd = wire_command(fn)
    if cmd is None:
        return None
    summary, param_docs = parse_doc(ast.get_docstring(fn))

    args = fn.args.args
    defaults = fn.args.defaults
    pad = [None] * (len(args) - len(defaults))
    params = []
    for arg, dflt in zip(args, pad + list(defaults)):
        if arg.arg in ("ctx", "self"):
            continue
        schema, nilable = ann_to_schema(arg.annotation)
        dval, has_default = literal(dflt)
        params.append({
            "name": kebab(arg.arg),
            "wire": arg.arg,
            "schema": schema,
            "nilable": nilable or (has_default and dval is None),
            "default": dval,
            "has_default": has_default,
            "doc": param_docs.get(arg.arg, ""),
        })
    return {
        "command": cmd,
        "tool": "gimp_" + fn.name,
        "fn": fn.name,
        "doc": summary,
        "params": params,
    }


# ---------------------------------------------------------------- EDN emitter

def edn_str(s: str) -> str:
    return json.dumps(s, ensure_ascii=False)


def edn_val(v):
    if v is None:
        return "nil"
    if isinstance(v, bool):
        return "true" if v else "false"
    if isinstance(v, str):
        return edn_str(v)
    if isinstance(v, (int, float)):
        return repr(v)
    if isinstance(v, list):
        return "[" + " ".join(edn_val(x) for x in v) + "]"
    if isinstance(v, dict):
        return "{" + " ".join(f"{edn_str(k)} {edn_val(x)}" for k, x in v.items()) + "}"
    return edn_str(str(v))


def emit(descs) -> str:
    out = [
        ";; GIMP MCP command contract : DERIVED, do not hand-edit.",
        ";; Regenerated by dev/extract_gimp_contract.py from the referential",
        ";; project gimp_mcp_server.py (MIT). Each entry is one GIMP-side command;",
        ";; the Clojure tool surface is generated from this data (OCP: add a row,",
        ";; get a tool, no new defn).",
        "",
        "[",
    ]
    for d in descs:
        out.append(" {:command " + edn_str(d["command"]))
        out.append("  :tool    " + edn_str(d["tool"]))
        out.append("  :doc     " + edn_str(d["doc"]))
        if not d["params"]:
            out.append("  :params  []}")
            continue
        out.append("  :params  [")
        for p in d["params"]:
            bits = [
                "{:name " + edn_str(p["name"]),
                ":wire " + edn_str(p["wire"]),
                ":schema " + p["schema"],
            ]
            if p["nilable"]:
                bits.append(":nilable? true")
            if p["has_default"]:
                bits.append(":default " + edn_val(p["default"]))
            else:
                bits.append(":required? true")
            if p["doc"]:
                bits.append(":doc " + edn_str(p["doc"]))
            out.append("            " + " ".join(bits) + "}")
        out.append("           ]}")
    out.append("]")
    return "\n".join(out) + "\n"


def main():
    server_src = SERVER.read_text()
    plugin_src = PLUGIN.read_text()

    tree = ast.parse(server_src)
    descs = []
    for node in tree.body:
        if isinstance(node, ast.FunctionDef) and is_mcp_tool(node):
            d = extract(node)
            if d:
                descs.append(d)

    handled = plugin_commands(plugin_src)
    sent = {d["command"] for d in descs}
    orphan_tools = sorted(sent - handled)     # tool sends what plugin ignores
    unused_cmds = sorted(handled - sent)      # plugin handles what no tool sends

    dest = Path(sys.argv[1]) if len(sys.argv) > 1 else Path("commands.edn")
    dest.parent.mkdir(parents=True, exist_ok=True)
    dest.write_text(emit(descs))

    print(f"tools extracted : {len(descs)}")
    print(f"plugin commands : {len(handled)}")
    print(f"params total    : {sum(len(d['params']) for d in descs)}")
    print(f"UNHANDLED (tool sends, plugin ignores): {orphan_tools}")
    print(f"UNEXPOSED (plugin handles, no tool)   : {unused_cmds}")
    print(f"wrote {dest}")


if __name__ == "__main__":
    main()
