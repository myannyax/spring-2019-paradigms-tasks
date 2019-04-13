import model


class PrettyPrinter(model.ASTNodeVisitor):
    number_of_tabs = 0
    is_ind = 1

    def visit_number(self, number):
        result = "\t" * PrettyPrinter.number_of_tabs + str(number.value) + \
                  ";" * PrettyPrinter.is_ind
        if PrettyPrinter.is_ind:
            result += "\n"
        return result

    def visit_conditional(self, conditional):
        result = "\t" * PrettyPrinter.number_of_tabs + "if ("
        result += buff(conditional.condition, is_ind=False)
        result += ") {" + "\n"
        if conditional.if_true:
            PrettyPrinter.number_of_tabs += 1
            for exp in conditional.if_true:
                result += buff(exp)
            PrettyPrinter.number_of_tabs -= 1
        result += "\t" * PrettyPrinter.number_of_tabs + "}"
        if conditional.if_false:
            result += " else {" + "\n"
            PrettyPrinter.number_of_tabs += 1
            for exp in conditional.if_false:
                result += buff(exp)
            PrettyPrinter.number_of_tabs -= 1
            result += "\t" * PrettyPrinter.number_of_tabs + "}"
        if PrettyPrinter.is_ind:
            result += "\n"
        return result

    def visit_print(self, printt):
        result = "\t" * PrettyPrinter.number_of_tabs + "print "
        result += buff(printt.expr, is_ind=False)
        result += ";" * PrettyPrinter.is_ind
        if PrettyPrinter.is_ind:
            result += "\n"
        return result

    def visit_read(self, read):
        result = "\t" * PrettyPrinter.number_of_tabs + \
                  "read " + str(read.name)
        result += ";" * PrettyPrinter.is_ind
        if PrettyPrinter.is_ind:
            result += "\n"
        return result

    def visit_reference(self, reference):
        result = "\t" * PrettyPrinter.number_of_tabs + reference.name
        result += ";" * PrettyPrinter.is_ind
        if PrettyPrinter.is_ind:
            result += "\n"
        return result

    def visit_function_definition(self, functiondefinition):
        result = "\t" * PrettyPrinter.number_of_tabs + \
                  "def " + functiondefinition.name
        result += "("
        for elem in functiondefinition.function.args[0:-1]:
            result += elem + ', '
        if functiondefinition.function.args:
            result += functiondefinition.function.args[-1]
        result += ") {" + "\n"
        PrettyPrinter.number_of_tabs += 1
        for exp in functiondefinition.function.body:
            result += buff(exp)
        PrettyPrinter.number_of_tabs -= 1
        result += "}"
        if PrettyPrinter.is_ind:
            result += "\n"
        return result

    def visit_function_call(self, functioncall):
        result = "\t" * PrettyPrinter.number_of_tabs
        result += buff(functioncall.fun_expr, is_ind=False)
        result += "("
        for elem in functioncall.args[0:-1]:
            result += buff(elem, is_ind=False)
            result += ", "
        if functioncall.args:
            result += buff(functioncall.args[-1], is_ind=False)
        result += ")"
        result += ";" * PrettyPrinter.is_ind
        if PrettyPrinter.is_ind:
            result += "\n"
        return result

    def visit_binary_operation(self, binaryoperation):
        result = "\t" * PrettyPrinter.number_of_tabs + "("
        result += buff(binaryoperation.lhs, is_ind=False)
        result += " " + binaryoperation.op + " "
        result += buff(binaryoperation.rhs, is_ind=False)
        result += ")"
        result += ";" * PrettyPrinter.is_ind
        if PrettyPrinter.is_ind:
            result += "\n"
        return result

    def visit_unary_operation(self, unaryoperation):
        result = "\t" * PrettyPrinter.number_of_tabs \
                  + "(" + unaryoperation.op + "("
        result += buff(unaryoperation.expr, is_ind=False)
        result += "))"
        result += ";" * PrettyPrinter.is_ind
        if PrettyPrinter.is_ind:
            result += "\n"
        return result


def buff(program, *args, is_ind=True):
    tabs = PrettyPrinter.number_of_tabs
    ind = PrettyPrinter.is_ind
    if not is_ind:
        PrettyPrinter.number_of_tabs = 0
        PrettyPrinter.is_ind = 0
    result = program.accept(PrettyPrinter())
    PrettyPrinter.number_of_tabs = tabs
    PrettyPrinter.is_ind = ind
    return result


def pretty_print(program):
    PrettyPrinter.number_of_tabs = 0
    PrettyPrinter.is_ind = 1
    print(buff(program, is_ind=True), end="")
