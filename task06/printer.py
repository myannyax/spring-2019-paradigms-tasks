import model


class PrettyPrinter(model.ASTNodeVisitor):
    def __init__(self):
        self.number_of_tabs = 0
        self.is_statement = True

    def add_leading_spaces(self):
        if self.is_statement:
            return "\t" * self.number_of_tabs
        else:
            return ""

    def end_line_if_statement(self, result):
        if self.is_statement:
            result += ";\n"
        return result

    def visit_number(self, number):
        result = self.add_leading_spaces()
        result += str(number.value)
        return self.end_line_if_statement(result)

    def visit_conditional(self, conditional):
        result = "\t" * self.number_of_tabs + "if ("
        result += self.format_(conditional.condition, is_instruction=False)
        result += ") {\n"
        if conditional.if_true:
            self.number_of_tabs += 1
            for exp in conditional.if_true:
                result += self.format_(exp)
            self.number_of_tabs -= 1
        result += "\t" * self.number_of_tabs + "}"
        if conditional.if_false:
            result += " else {" + "\n"
            self.number_of_tabs += 1
            for exp in conditional.if_false:
                result += self.format_(exp)
            self.number_of_tabs -= 1
            result += "\t" * self.number_of_tabs + "}"
        result += "\n"
        return result

    def visit_print(self, printt):
        result = "\t" * self.number_of_tabs + "print "
        result += self.format_(printt.expr, is_instruction=False)
        return self.end_line_if_statement(result)

    def visit_read(self, read):
        result = "\t" * self.number_of_tabs + \
                  "read " + str(read.name)
        return self.end_line_if_statement(result)

    def visit_reference(self, reference):
        result = self.add_leading_spaces()
        result += reference.name
        return self.end_line_if_statement(result)

    def visit_function_definition(self, function_definition):
        result = "\t" * self.number_of_tabs + \
                  "def " + function_definition.name
        result += "("
        for elem in function_definition.function.args[0:-1]:
            result += elem + ', '
        if function_definition.function.args:
            result += function_definition.function.args[-1]
        result += ") {\n"
        self.number_of_tabs += 1
        for exp in function_definition.function.body:
            result += self.format_(exp)
        self.number_of_tabs -= 1
        result += "\t" * self.number_of_tabs + "}\n"
        return result

    def visit_function_call(self, function_call):
        result = self.add_leading_spaces()
        result += self.format_(function_call.fun_expr, is_instruction=False)
        result += "("
        for elem in function_call.args[0:-1]:
            result += self.format_(elem, is_instruction=False)
            result += ", "
        if function_call.args:
            result += self.format_(function_call.args[-1], is_instruction=False)
        result += ")"
        return self.end_line_if_statement(result)

    def visit_binary_operation(self, binary_operation):
        result = self.add_leading_spaces()
        result += "("
        result += self.format_(binary_operation.lhs, is_instruction=False)
        result += " " + binary_operation.op + " "
        result += self.format_(binary_operation.rhs, is_instruction=False)
        result += ")"
        return self.end_line_if_statement(result)

    def visit_unary_operation(self, unary_operation):
        result = self.add_leading_spaces()
        result += "(" + unary_operation.op
        result += self.format_(unary_operation.expr, is_instruction=False)
        result += ")"
        return self.end_line_if_statement(result)

    def format_(self, program, *, is_instruction=True):
        new_printer = PrettyPrinter()
        new_printer.is_statement = is_instruction
        new_printer.number_of_tabs = self.number_of_tabs
        return program.accept(new_printer)


def pretty_print(program):
    printer = PrettyPrinter()
    print(printer.format_(program, is_instruction=True), end="")
