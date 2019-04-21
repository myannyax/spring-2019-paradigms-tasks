import model


class PrettyPrinter(model.ASTNodeVisitor):
    number_of_tabs = 0
    is_instruction = True

    def visit_number(self, number):
        if PrettyPrinter.is_instruction:
            result = "\t" * PrettyPrinter.number_of_tabs
        else:
            result = ""
        result += str(number.value)
        if PrettyPrinter.is_instruction:
            result += ";\n"
        return result

    def visit_conditional(self, conditional):
        result = "\t" * PrettyPrinter.number_of_tabs + "if ("
        result += format_(conditional.condition, is_instruction=False)
        result += ") {\n"
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
        result += "\n"
        return result

    def visit_print(self, printt):
        result = "\t" * PrettyPrinter.number_of_tabs + "print "
        result += format_(printt.expr, is_instruction=False)
        result += ";\n"
        return result

    def visit_read(self, read):
        result = "\t" * PrettyPrinter.number_of_tabs + \
                  "read " + str(read.name)
        result += ";\n"
        return result

    def visit_reference(self, reference):
        if PrettyPrinter.is_instruction:
            result = "\t" * PrettyPrinter.number_of_tabs
        else:
            result = ""
        result += reference.name
        if PrettyPrinter.is_instruction:
            result += ";\n"
        return result

    def visit_function_definition(self, functiondefinition):
        result = "\t" * PrettyPrinter.number_of_tabs + \
                  "def " + functiondefinition.name
        result += "("
        for elem in functiondefinition.function.args[0:-1]:
            result += elem + ', '
        if functiondefinition.function.args:
            result += functiondefinition.function.args[-1]
        result += ") {\n"
        PrettyPrinter.number_of_tabs += 1
        for exp in functiondefinition.function.body:
            result += format_(exp)
        PrettyPrinter.number_of_tabs -= 1
        result += "}\n"
        return result

    def visit_function_call(self, functioncall):
        if PrettyPrinter.is_instruction:
            result = "\t" * PrettyPrinter.number_of_tabs
        else:
            result = ""
        result += format_(functioncall.fun_expr, is_instruction=False)
        result += "("
        for elem in functioncall.args[0:-1]:
            result += format_(elem, is_instruction=False)
            result += ", "
        if functioncall.args:
            result += format_(functioncall.args[-1], is_instruction=False)
        result += ")"
        if PrettyPrinter.is_instruction:
            result += ";\n"
        return result

    def visit_binary_operation(self, binaryoperation):
        if PrettyPrinter.is_instruction:
            result = "\t" * PrettyPrinter.number_of_tabs
        else:
            result = ""
        result += "("
        result += format_(binaryoperation.lhs, is_instruction=False)
        result += " " + binaryoperation.op + " "
        result += format_(binaryoperation.rhs, is_instruction=False)
        result += ")"
        if PrettyPrinter.is_instruction:
            result += ";\n"
        return result

    def visit_unary_operation(self, unaryoperation):
        if PrettyPrinter.is_instruction:
            result = "\t" * PrettyPrinter.number_of_tabs
        else:
            result = ""
        result += "(" + unaryoperation.op
        result += format_(unaryoperation.expr, is_instruction=False)
        result += ")"
        if PrettyPrinter.is_instruction:
            result += ";\n"
        return result


def format_(program, *args, is_instruction=True):
    instr = PrettyPrinter.is_instruction
    PrettyPrinter.is_instruction = is_instruction
    result = program.accept(PrettyPrinter())
    PrettyPrinter.is_instruction = instr
    return result


def pretty_print(program):
    PrettyPrinter.number_of_tabs = 0
    PrettyPrinter.is_instruction = 1
    print(format_(program, is_instruction=True), end="")
