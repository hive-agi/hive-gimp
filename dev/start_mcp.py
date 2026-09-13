"""Start the GIMP MCP plugin's socket server from a GIMP batch run.

Headless, so a live end-to-end needs no GUI and no click:

    flatpak run org.gimp.GIMP -n -i -d -f \
      --batch-interpreter python-fu-eval \
      -b "exec(open('dev/start_mcp.py').read())"

Two details make this work, and both cost an hour to find:

  * GIMP 3's PDB has lookup_procedure, NOT run_procedure. Calling the latter
    raises AttributeError and the batch stops before the plugin is ever asked
    for.
  * The plugin's run() blocks in a GLib.MainLoop and never returns, so nothing
    here has to keep the process alive. A sleep loop would be wrong: it would
    outlive a server that had already died.

The registration probe is not decoration. A plugin installed in the wrong
directory is silent, and every later command then fails as an unknown command
far from the real cause. Ask GIMP where it looks, with Gimp.directory().
"""

from gi.repository import Gimp

pdb = Gimp.get_pdb()
names = ["plug-in-mcp-server", "plug-in-mcp-check", "plug-in-mcp-restart"]

print("MCP-USER-DIR:", Gimp.directory(), flush=True)
print("MCP-PROCS-REGISTERED:", [n for n in names if pdb.procedure_exists(n)], flush=True)

proc = pdb.lookup_procedure("plug-in-mcp-server")
if proc is None:
    print(
        "MCP-FATAL: plug-in-mcp-server is not registered. Install "
        "gimp-mcp-plugin.py at <MCP-USER-DIR above>/plug-ins/gimp-mcp-plugin/"
        "gimp-mcp-plugin.py and chmod +x it.",
        flush=True,
    )
else:
    cfg = proc.create_config()
    cfg.set_property("run-mode", Gimp.RunMode.NONINTERACTIVE)
    print("MCP-STARTING: blocking in GLib.MainLoop", flush=True)
    proc.run(cfg)
