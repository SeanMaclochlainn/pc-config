import pdb

class Config(pdb.DefaultConfig):
    sticky_by_default = True
    current_line_color = "39;49;20"

    def setup(self, pdb):
        # Save history across sessions
        import readline
        from pathlib import Path
        histfile = Path("~/.pdb-pyhist").expanduser()
        try:
            readline.read_history_file(histfile)
        except IOError:
            pass
        import atexit
        atexit.register(readline.write_history_file, histfile)
        readline.set_history_length(500)

#don't think this is needed
# _pdbrc_init()
# del _pdbrc_init
