from typing import Any, Callable


class NotAMenuOption(Exception):
    pass


class Menu:
    title: str
    opts: dict[int, str]
    opts_with_funcs: dict[int, Callable]
    funcs: dict[int, Callable]
    current_opt: int = -1
    exit_opt: int

    def __init__(self, opts_and_funcs: dict[str, Any], *, title: str = "Menù"):
        self.title = f"{title}:"
        opts = list(opts_and_funcs.keys())
        funcs = opts_and_funcs.values()
        opts_with_number = {i + 1: opt for (i, opt) in enumerate(opts)}
        self.opts_with_funcs = {i + 1: func for (i, func) in enumerate(funcs)}
        self.exit_opt = len(opts_with_number) + 1
        opts_with_number[self.exit_opt] = "Exit"
        self.opts = opts_with_number

    def _input_option(self):
        print("\nScegli un'opzione:")
        while True:
            option = input()
            try:
                as_num = int(option)
                if as_num not in self.opts.keys():
                    raise NotAMenuOption
                return as_num
            except ValueError:
                print("Non un numero! Riprova.")
            except NotAMenuOption:
                print("Non una delle opzioni! Riprova.")

    def run(self):
        print()
        while True:
            print(self.title)
            for k, v in self.opts.items():
                print(f"{k}. {v}")
            opt = self._input_option()
            if opt == self.exit_opt:
                return
            else:
                print()
                self.opts_with_funcs[opt]()
                print()
