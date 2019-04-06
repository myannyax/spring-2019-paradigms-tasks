import model


class PrettyPrinter(model.ASTNodeVisitor):
    number_of_tabs = 0
    is_ind = 1

    def visit_number(self, number):
        print("\t" * PrettyPrinter.number_of_tabs + str(number.value) +
              ";" * PrettyPrinter.is_ind, end="")
        if PrettyPrinter.is_ind:
            print()

    def visit_conditional(self, conditional):
        print("\t" * PrettyPrinter.number_of_tabs + "if (", end="")
        pretty_print(conditional.condition, is_ind=False)
        print(") {")
        if conditional.if_true:
            PrettyPrinter.number_of_tabs += 1
            for exp in conditional.if_true:
                pretty_print(exp)
            PrettyPrinter.number_of_tabs -= 1
        print("\t" * PrettyPrinter.number_of_tabs + "}", end="")
        if conditional.if_false:
            print(" else {")
            PrettyPrinter.number_of_tabs += 1
            for exp in conditional.if_false:
                pretty_print(exp)
            PrettyPrinter.number_of_tabs -= 1
            print("\t" * PrettyPrinter.number_of_tabs + "}", end="")
        if PrettyPrinter.is_ind:
            print()

    def visit_print(self, printt):
        print("\t" * PrettyPrinter.number_of_tabs + "print ", end="")
        pretty_print(printt.expr, is_ind=False)
        print(";" * PrettyPrinter.is_ind, end="")
        if PrettyPrinter.is_ind:
            print()

    def visit_read(self, read):
        print("\t" * PrettyPrinter.number_of_tabs + "read " + str(read.name), end="")
        print(";" * PrettyPrinter.is_ind, end="")
        if PrettyPrinter.is_ind:
            print()

    def visit_reference(self, reference):
        print("\t" * PrettyPrinter.number_of_tabs + reference.name, end="")
        print(";" * PrettyPrinter.is_ind, end="")
        if PrettyPrinter.is_ind:
            print()

    def visit_functiondefinition(self, functiondefinition):
        print("\t" * PrettyPrinter.number_of_tabs + "def "
              + functiondefinition.name, end="")
        print("(", end="")
        for arg in functiondefinition.function.args:
            pretty_print(arg, is_ind=False)
            print(", ", end="")
        print(") {")
        PrettyPrinter.number_of_tabs += 1
        for exp in functiondefinition.function.body:
            pretty_print(exp)
        PrettyPrinter.number_of_tabs -= 1
        print("}", end="")
        if PrettyPrinter.is_ind:
            print()

    def visit_functioncall(self, functioncall):
        print("\t" * PrettyPrinter.number_of_tabs, end="")
        pretty_print(functioncall.fun_expr, is_ind=False)
        print("(", end="")
        for i in range(len(functioncall.args)):
            pretty_print(functioncall.args[i], is_ind=False)
            if i != len(functioncall.args) - 1:
                print(", ", end="")
        print(")", end="")
        print(";" * PrettyPrinter.is_ind, end="")
        if PrettyPrinter.is_ind:
            print()

    def visit_binaryoperation(self, binaryoperation):
        print("\t" * PrettyPrinter.number_of_tabs + "(", end="")
        pretty_print(binaryoperation.lhs, is_ind=False)
        print(" " + binaryoperation.op + " ", end="")
        pretty_print(binaryoperation.rhs, is_ind=False)
        print(")", end="")
        print(";" * PrettyPrinter.is_ind, end="")
        if PrettyPrinter.is_ind:
            print()

    def visit_unaryoperation(self, unaryoperation):
        print("\t" * PrettyPrinter.number_of_tabs + "("
              + unaryoperation.op + "(", end="")
        pretty_print(unaryoperation.expr, is_ind=False)
        print("))", end="")
        print(";" * PrettyPrinter.is_ind, end="")
        if PrettyPrinter.is_ind:
            print()


def pretty_print(program, *args, is_ind=True):
    tabs = PrettyPrinter.number_of_tabs
    ind = PrettyPrinter.is_ind
    if not is_ind:
        PrettyPrinter.number_of_tabs = 0
        PrettyPrinter.is_ind = 0
    program.accept(PrettyPrinter())
    PrettyPrinter.number_of_tabs = tabs
    PrettyPrinter.is_ind = ind
