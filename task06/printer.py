import model


class PrettyPrinter(model.ASTNodeVisitor):
    number_of_tabs = 0
    is_instruction = 1

    def visit_number(self, number):
        result = "\t" * PrettyPrinter.number_of_tabs + str(number.value) + \
                  ";" * PrettyPrinter.is_instruction
        if PrettyPrinter.is_instruction:
            result += "\n"
        return result

    def visit_conditional(self, conditional):
        result = "\t" * PrettyPrinter.number_of_tabs + "if ("
        result += format_(conditional.condition, is_instruction=False)
        result += ") {" + "\n"
        if conditional.if_true:
            PrettyPrinter.number_of_tabs += 1
            for exp in conditional.if_true:
                result += format_(exp)
            PrettyPrinter.number_of_tabs -= 1
        result += "\t" * PrettyPrinter.number_of_tabs + "}"
        if conditional.if_false:
            result += " else {" + "\n"
            PrettyPrinter.number_of_tabs += 1
            for exp in conditional.if_false:
                result += format_(exp)
            PrettyPrinter.number_of_tabs -= 1
            result += "\t" * PrettyPrinter.number_of_tabs + "}"
        if PrettyPrinter.is_instruction:
            result += "\n"
        return result

    def visit_print(self, printt):
        result = "\t" * PrettyPrinter.number_of_tabs + "print "
        result += format_(printt.expr, is_instruction=False)
        result += ";" * PrettyPrinter.is_instruction
        if PrettyPrinter.is_instruction:
            result += "\n"
        return result

    def visit_read(self, read):
        result = "\t" * PrettyPrinter.number_of_tabs + \
                  "read " + str(read.name)
        result += ";" * PrettyPrinter.is_instruction
        if PrettyPrinter.is_instruction:
            result += "\n"
        return result

    def visit_reference(self, reference):
        result = "\t" * PrettyPrinter.number_of_tabs + reference.name
        result += ";" * PrettyPrinter.is_instruction
        if PrettyPrinter.is_instruction:
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
            result += format_(exp)
        PrettyPrinter.number_of_tabs -= 1
        result += "}"
        if PrettyPrinter.is_instruction:
            result += "\n"
        return result

    def visit_function_call(self, functioncall):
        result = "\t" * PrettyPrinter.number_of_tabs
        result += format_(functioncall.fun_expr, is_instruction=False)
        result += "("
        for elem in functioncall.args[0:-1]:
            result += format_(elem, is_instruction=False)
            result += ", "
        if functioncall.args:
            result += format_(functioncall.args[-1], is_instruction=False)
        result += ")"
        result += ";" * PrettyPrinter.is_instruction
        if PrettyPrinter.is_instruction:
            result += "\n"
        return result

    def visit_binary_operation(self, binaryoperation):
        result = "\t" * PrettyPrinter.number_of_tabs + "("
        result += format_(binaryoperation.lhs, is_instruction=False)
        result += " " + binaryoperation.op + " "
        result += format_(binaryoperation.rhs, is_instruction=False)
        result += ")"
        result += ";" * PrettyPrinter.is_instruction
        if PrettyPrinter.is_instruction:
            result += "\n"
        return result

    def visit_unary_operation(self, unaryoperation):
        result = "\t" * PrettyPrinter.number_of_tabs \
                  + "(" + unaryoperation.op + "("
        result += format_(unaryoperation.expr, is_instruction=False)
        result += "))"
        result += ";" * PrettyPrinter.is_instruction
        if PrettyPrinter.is_instruction:
            result += "\n"
        return result


def format_(program, *args, is_instruction=True):
    tabs = PrettyPrinter.number_of_tabs
    ind = PrettyPrinter.is_instruction
    if not is_instruction:
        PrettyPrinter.number_of_tabs = 0
        PrettyPrinter.is_instruction = 0
    result = program.accept(PrettyPrinter())
    PrettyPrinter.number_of_tabs = tabs
    PrettyPrinter.is_instruction = ind
    return result


def pretty_print(program):
    PrettyPrinter.number_of_tabs = 0
    PrettyPrinter.is_instruction = 1
    print(format_(program, is_instruction=True), end="")
