import pdb
import readline
from pathlib import Path
import atexit


def clean_history_entries(history_file_path):
    with open(history_file_path, "r") as history_file:
        lines = history_file.readlines()

    # exclude q,c and ll entries
    new_lines = [line for line in lines if line not in ("q\n", "c\n", "ll\n")]

    with open(history_file_path, "w") as history_file:
        history_file.writelines(new_lines)


class Config(pdb.DefaultConfig):
    sticky_by_default = True
    current_line_color = "39;49;20"
    highlight = True
    show_traceback_on_error = True

    def setup(self, pdb):
        gdrive_history_path = Path(
            "~/gdrive/programming/python/.pdb-pyhist"
        ).expanduser()
        if gdrive_history_path.exists():
            histfile = gdrive_history_path
        else:
            histfile = Path("~/.pdb-pyhist").expanduser()
        readline.read_history_file(histfile)

        atexit.register(clean_history_entries, histfile)
        atexit.register(readline.write_history_file, histfile)

        readline.set_history_length(10000)
