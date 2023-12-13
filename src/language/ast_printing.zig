const std = @import("std");

const ast = @import("ast.zig");

fn printIndentation(writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    for (0..indentation) |_| try writer.print("  ", .{});
}

fn print(string: []const u8, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try printIndentation(writer, indentation);
    try writer.print("{s}\n", .{string});
}

pub fn printParenthesizedExpression(node: ast.ParenthesizedExpression, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("ParenthesizedExpression", writer, indentation);
    try printExpression(node.expression.*, writer, indentation + 1);
}

pub fn printIdentifierReference(node: ast.IdentifierReference, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("IdentifierReference", writer, indentation);
    try print(node.identifier, writer, indentation + 1);
}

pub fn printPrimaryExpression(node: ast.PrimaryExpression, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    // Omit printing 'PrimaryExpression' here, it's implied and only adds nesting.
    switch (node) {
        .this => try print("this", writer, indentation),
        .identifier_reference => |x| try printIdentifierReference(x, writer, indentation),
        .literal => |x| try printLiteral(x, writer, indentation),
        .array_literal => |x| try printArrayLiteral(x, writer, indentation),
        .object_literal => |x| try printObjectLiteral(x, writer, indentation),
        .function_expression => |x| try printFunctionExpression(x, writer, indentation),
        .class_expression => |x| try printClassExpression(x, writer, indentation),
        .generator_expression => |x| try printGeneratorExpression(x, writer, indentation),
        .async_function_expression => |x| try printAsyncFunctionExpression(x, writer, indentation),
        .async_generator_expression => |x| try printAsyncGeneratorExpression(x, writer, indentation),
        .regular_expression_literal => |x| try printRegularExpressionLiteral(x, writer, indentation),
        .template_literal => |x| try printTemplateLiteral(x, writer, indentation),
        .arrow_function => |x| try printArrowFunction(x, writer, indentation),
        .async_arrow_function => |x| try printAsyncArrowFunction(x, writer, indentation),
        .parenthesized_expression => |x| try printParenthesizedExpression(x, writer, indentation),
    }
}

pub fn printMemberExpression(node: ast.MemberExpression, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("MemberExpression", writer, indentation);
    try print("expression:", writer, indentation + 1);
    try printExpression(node.expression.*, writer, indentation + 2);
    try print("property:", writer, indentation + 1);
    switch (node.property) {
        .expression => |expression| try printExpression(expression.*, writer, indentation + 2),
        .identifier => |identifier| try print(identifier, writer, indentation + 2),
    }
}

pub fn printSuperProperty(node: ast.SuperProperty, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("SuperProperty", writer, indentation);
    switch (node) {
        .expression => |expression| try printExpression(expression.*, writer, indentation + 1),
        .identifier => |identifier| try print(identifier, writer, indentation + 1),
    }
}

pub fn printMetaProperty(node: ast.MetaProperty, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("MetaProperty", writer, indentation);
    switch (node) {
        .new_target => try print("new.target", writer, indentation + 1),
        .import_meta => try print("import.meta", writer, indentation + 1),
    }
}

pub fn printNewExpression(node: ast.NewExpression, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("NewExpression", writer, indentation);
    try print("expression:", writer, indentation + 1);
    try printExpression(node.expression.*, writer, indentation + 2);
    try print("arguments:", writer, indentation + 1);
    for (node.arguments) |argument| {
        try printExpression(argument, writer, indentation + 2);
    }
}

pub fn printCallExpression(node: ast.CallExpression, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("CallExpression", writer, indentation);
    try print("expression:", writer, indentation + 1);
    try printExpression(node.expression.*, writer, indentation + 2);
    try print("arguments:", writer, indentation + 1);
    for (node.arguments) |argument| {
        try printExpression(argument, writer, indentation + 2);
    }
}

pub fn printSuperCall(node: ast.SuperCall, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("SuperCall", writer, indentation);
    for (node.arguments) |argument| {
        try printExpression(argument, writer, indentation + 1);
    }
}

pub fn printImportCall(node: ast.ImportCall, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("ImportCall", writer, indentation);
    try printExpression(node.expression.*, writer, indentation + 1);
}

pub fn printOptionalExpression(node: ast.OptionalExpression, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("OptionalExpression", writer, indentation);
    try print("expression:", writer, indentation + 1);
    try printExpression(node.expression.*, writer, indentation + 2);
    try print("property:", writer, indentation + 1);
    switch (node.property) {
        .expression => |expression| try printExpression(expression.*, writer, indentation + 2),
        .identifier => |identifier| try print(identifier, writer, indentation + 2),
        .arguments => |arguments| {
            try print("arguments:", writer, indentation + 2);
            for (arguments) |argument| {
                try printExpression(argument, writer, indentation + 3);
            }
        },
    }
}

pub fn printLiteral(node: ast.Literal, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("Literal", writer, indentation);
    switch (node) {
        .null => try print("null", writer, indentation + 1),
        .boolean => |boolean| try print(if (boolean) "true" else "false", writer, indentation + 1),
        .numeric => |numeric_literal| try print(numeric_literal.text, writer, indentation + 1),
        .string => |string_literal| try print(string_literal.text, writer, indentation + 1),
    }
}

pub fn printArrayLiteral(node: ast.ArrayLiteral, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("ArrayLiteral", writer, indentation);
    for (node.element_list) |element| switch (element) {
        .elision => try print("<elision>", writer, indentation + 1),
        .expression => |expression| try printExpression(expression, writer, indentation + 1),
        .spread => |expression| {
            try print("...", writer, indentation + 1);
            try printExpression(expression, writer, indentation + 1);
        },
    };
}

pub fn printObjectLiteral(node: ast.ObjectLiteral, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("ObjectLiteral", writer, indentation);
    try printPropertyDefinitionList(node.property_definition_list, writer, indentation + 1);
}

pub fn printPropertyDefinitionList(node: ast.PropertyDefinitionList, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    // Omit printing 'PropertyDefinitionList' here, it's implied and only adds nesting.
    for (node.items) |property_definition| {
        try printPropertyDefinition(property_definition, writer, indentation);
    }
}

pub fn printPropertyDefinition(node: ast.PropertyDefinition, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    // Omit printing 'PropertyDefinition' here, it's implied and only adds nesting.
    switch (node) {
        .spread => |expression| {
            try print("...", writer, indentation);
            try printExpression(expression, writer, indentation);
        },
        .identifier_reference => |identifier_reference| {
            try printIdentifierReference(identifier_reference, writer, indentation);
        },
        .property_name_and_expression => |property_name_and_expression| {
            try print("property_name_and_expression:", writer, indentation);
            try printPropertyName(property_name_and_expression.property_name, writer, indentation + 1);
            try printExpression(property_name_and_expression.expression, writer, indentation + 1);
        },
        .method_definition => |method_definition| {
            try print("MethodDefinition", writer, indentation);
            try printMethodDefinition(method_definition, writer, indentation);
        },
    }
}

pub fn printPropertyName(node: ast.PropertyName, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("PropertyName", writer, indentation);
    switch (node) {
        .literal_property_name => |literal| switch (literal) {
            .identifier => |identifier| try print(identifier, writer, indentation + 1),
            .string_literal => |string_literal| try print(string_literal.text, writer, indentation + 1),
            .numeric_literal => |numeric_literal| try print(numeric_literal.text, writer, indentation + 1),
        },
        .computed_property_name => |expression| try printExpression(expression, writer, indentation + 1),
    }
}

pub fn printRegularExpressionLiteral(node: ast.RegularExpressionLiteral, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("RegularExpressionLiteral", writer, indentation);
    try print("pattern:", writer, indentation + 1);
    try print(node.pattern, writer, indentation + 2);
    try print("flags:", writer, indentation + 1);
    try print(node.flags, writer, indentation + 2);
}

pub fn printTemplateLiteral(node: ast.TemplateLiteral, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("TemplateLiteral", writer, indentation);
    try print(node.text, writer, indentation + 1);
}

pub fn printUpdateExpression(node: ast.UpdateExpression, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("UpdateExpression", writer, indentation);
    try print("type:", writer, indentation + 1);
    try print(@tagName(node.type), writer, indentation + 2);
    try print("operator:", writer, indentation + 1);
    try print(@tagName(node.operator), writer, indentation + 2);
    try print("expression:", writer, indentation + 1);
    try printExpression(node.expression.*, writer, indentation + 2);
}

pub fn printUnaryExpression(node: ast.UnaryExpression, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("UnaryExpression", writer, indentation);
    try print("operator:", writer, indentation + 1);
    try print(@tagName(node.operator), writer, indentation + 2);
    try print("expression:", writer, indentation + 1);
    try printExpression(node.expression.*, writer, indentation + 2);
}

pub fn printBinaryExpression(node: ast.BinaryExpression, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("BinaryExpression", writer, indentation);
    try printExpression(node.lhs_expression.*, writer, indentation + 1);
    try print(@tagName(node.operator), writer, indentation + 1);
    try printExpression(node.rhs_expression.*, writer, indentation + 1);
}

pub fn printRelationalExpression(node: ast.RelationalExpression, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("RelationalExpression", writer, indentation);
    try printExpression(node.lhs_expression.*, writer, indentation + 1);
    try print(@tagName(node.operator), writer, indentation + 1);
    try printExpression(node.rhs_expression.*, writer, indentation + 1);
}

pub fn printEqualityExpression(node: ast.EqualityExpression, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("EqualityExpression", writer, indentation);
    try printExpression(node.lhs_expression.*, writer, indentation + 1);
    try print(@tagName(node.operator), writer, indentation + 1);
    try printExpression(node.rhs_expression.*, writer, indentation + 1);
}

pub fn printLogicalExpression(node: ast.LogicalExpression, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("LogicalExpression", writer, indentation);
    try printExpression(node.lhs_expression.*, writer, indentation + 1);
    try print(@tagName(node.operator), writer, indentation + 1);
    try printExpression(node.rhs_expression.*, writer, indentation + 1);
}

pub fn printConditionalExpression(node: ast.ConditionalExpression, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("ConditionalExpression", writer, indentation);
    try print("test:", writer, indentation + 1);
    try printExpression(node.test_expression.*, writer, indentation + 2);
    try print("consequent:", writer, indentation + 1);
    try printExpression(node.consequent_expression.*, writer, indentation + 2);
    try print("alternate:", writer, indentation + 1);
    try printExpression(node.alternate_expression.*, writer, indentation + 2);
}

pub fn printAssignmentExpression(node: ast.AssignmentExpression, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("AssignmentExpression", writer, indentation);
    try printExpression(node.lhs_expression.*, writer, indentation + 1);
    try print(@tagName(node.operator), writer, indentation + 1);
    try printExpression(node.rhs_expression.*, writer, indentation + 1);
}

pub fn printSequenceExpression(node: ast.SequenceExpression, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("SequenceExpression", writer, indentation);
    for (node.expressions) |expression| {
        try printExpression(expression, writer, indentation + 1);
    }
}

pub fn printExpression(node: ast.Expression, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("Expression", writer, indentation);
    switch (node) {
        .primary_expression => |x| try printPrimaryExpression(x, writer, indentation + 1),
        .member_expression => |x| try printMemberExpression(x, writer, indentation + 1),
        .super_property => |x| try printSuperProperty(x, writer, indentation + 1),
        .meta_property => |x| try printMetaProperty(x, writer, indentation + 1),
        .new_expression => |x| try printNewExpression(x, writer, indentation + 1),
        .call_expression => |x| try printCallExpression(x, writer, indentation + 1),
        .super_call => |x| try printSuperCall(x, writer, indentation + 1),
        .import_call => |x| try printImportCall(x, writer, indentation + 1),
        .optional_expression => |x| try printOptionalExpression(x, writer, indentation + 1),
        .update_expression => |x| try printUpdateExpression(x, writer, indentation + 1),
        .unary_expression => |x| try printUnaryExpression(x, writer, indentation + 1),
        .binary_expression => |x| try printBinaryExpression(x, writer, indentation + 1),
        .relational_expression => |x| try printRelationalExpression(x, writer, indentation + 1),
        .equality_expression => |x| try printEqualityExpression(x, writer, indentation + 1),
        .logical_expression => |x| try printLogicalExpression(x, writer, indentation + 1),
        .conditional_expression => |x| try printConditionalExpression(x, writer, indentation + 1),
        .assignment_expression => |x| try printAssignmentExpression(x, writer, indentation + 1),
        .sequence_expression => |x| try printSequenceExpression(x, writer, indentation + 1),
    }
}

pub fn printStatement(node: ast.Statement, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("Statement", writer, indentation);
    switch (node) {
        .block_statement => |x| try printBlockStatement(x, writer, indentation + 1),
        .variable_statement => |x| try printVariableStatement(x, writer, indentation + 1),
        .empty_statement => try print("empty", writer, indentation + 1),
        .expression_statement => |x| try printExpressionStatement(x, writer, indentation + 1),
        .if_statement => |x| try printIfStatement(x, writer, indentation + 1),
        .breakable_statement => |x| try printBreakableStatement(x, writer, indentation + 1),
        .continue_statement => |x| try printContinueStatement(x, writer, indentation + 1),
        .break_statement => |x| try printBreakStatement(x, writer, indentation + 1),
        .return_statement => |x| try printReturnStatement(x, writer, indentation + 1),
        .throw_statement => |x| try printThrowStatement(x, writer, indentation + 1),
        .try_statement => |x| try printTryStatement(x, writer, indentation + 1),
        .debugger_statement => try print("debugger", writer, indentation + 1),
    }
}

pub fn printDeclaration(node: ast.Declaration, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("Declaration", writer, indentation);
    switch (node) {
        .hoistable_declaration => |x| try printHoistableDeclaration(x, writer, indentation + 1),
        .class_declaration => |x| try printClassDeclaration(x, writer, indentation + 1),
        .lexical_declaration => |x| try printLexicalDeclaration(x, writer, indentation + 1),
    }
}

pub fn printHoistableDeclaration(node: ast.HoistableDeclaration, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    // Omit printing 'HoistableDeclaration' here, it's implied and only adds nesting.
    switch (node) {
        .function_declaration => |x| try printFunctionDeclaration(x, writer, indentation + 1),
        .generator_declaration => |x| try printGeneratorDeclaration(x, writer, indentation + 1),
        .async_function_declaration => |x| try printAsyncFunctionDeclaration(x, writer, indentation + 1),
        .async_generator_declaration => |x| try printAsyncGeneratorDeclaration(x, writer, indentation + 1),
    }
}

pub fn printBreakableStatement(node: ast.BreakableStatement, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    // Omit printing 'BreakableStatement' here, it's implied and only adds nesting.
    switch (node) {
        .iteration_statement => |x| try printIterationStatement(x, writer, indentation),
    }
}

pub fn printBlockStatement(node: ast.BlockStatement, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    // Omit printing 'BlockStatement' here, it's implied and only adds nesting.
    try printBlock(node.block, writer, indentation);
}

pub fn printBlock(node: ast.Block, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("Block", writer, indentation);
    try printStatementList(node.statement_list, writer, indentation + 1);
}

pub fn printStatementList(node: ast.StatementList, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    // Omit printing 'StatementList' here, it's implied and only adds nesting.
    for (node.items) |item| {
        try printStatementListItem(item, writer, indentation);
    }
}

pub fn printStatementListItem(node: ast.StatementListItem, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    // Omit printing 'StatementListItem' here, it's implied and only adds nesting.
    switch (node) {
        .statement => |statement| try printStatement(statement.*, writer, indentation),
        .declaration => |declaration| try printDeclaration(declaration.*, writer, indentation),
    }
}

pub fn printLexicalDeclaration(node: ast.LexicalDeclaration, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("LexicalDeclaration", writer, indentation);
    try print(@tagName(node.type), writer, indentation + 1);
    try printBindingList(node.binding_list, writer, indentation + 1);
}

pub fn printBindingList(node: ast.BindingList, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    // Omit printing 'BindingList' here, it's implied and only adds nesting.
    for (node.items) |lexical_binding| {
        try printLexicalBinding(lexical_binding, writer, indentation);
    }
}

pub fn printLexicalBinding(node: ast.LexicalBinding, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("LexicalBinding", writer, indentation);
    try print("binding_identifier:", writer, indentation + 1);
    try print(node.binding_identifier, writer, indentation + 2);
    if (node.initializer) |initializer| {
        try print("initializer:", writer, indentation + 1);
        try printExpression(initializer, writer, indentation + 2);
    }
}

pub fn printVariableStatement(node: ast.VariableStatement, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("VariableStatement", writer, indentation);
    try printVariableDeclarationList(node.variable_declaration_list, writer, indentation + 1);
}

pub fn printVariableDeclarationList(node: ast.VariableDeclarationList, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    // Omit printing 'VariableDeclarationList' here, it's implied and only adds nesting.
    for (node.items) |variable_declaration| {
        try printVariableDeclaration(variable_declaration, writer, indentation);
    }
}

pub fn printVariableDeclaration(node: ast.VariableDeclaration, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("VariableDeclaration", writer, indentation);
    try print("binding_identifier:", writer, indentation + 1);
    try print(node.binding_identifier, writer, indentation + 2);
    if (node.initializer) |initializer| {
        try print("initializer:", writer, indentation + 1);
        try printExpression(initializer, writer, indentation + 2);
    }
}

pub fn printBindingElement(node: ast.BindingElement, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("BindingElement", writer, indentation);
    try print(node.identifier, writer, indentation + 1);
    if (node.initializer) |initializer| {
        try print("initializer:", writer, indentation + 1);
        try printExpression(initializer, writer, indentation + 2);
    }
}

pub fn printBindingRestElement(node: ast.BindingRestElement, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("BindingRestElement", writer, indentation);
    try print(node.identifier, writer, indentation + 1);
}

pub fn printExpressionStatement(node: ast.ExpressionStatement, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    // Omit printing 'ExpressionStatement' here, it's implied and only adds nesting.
    try printExpression(node.expression, writer, indentation);
}

pub fn printIfStatement(node: ast.IfStatement, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("IfStatement", writer, indentation);
    try print("test:", writer, indentation + 1);
    try printExpression(node.test_expression, writer, indentation + 2);
    try print("consequent:", writer, indentation + 1);
    try printStatement(node.consequent_statement.*, writer, indentation + 2);
    if (node.alternate_statement) |alternate_statement| {
        try print("alternate:", writer, indentation + 1);
        try printStatement(alternate_statement.*, writer, indentation + 2);
    }
}

pub fn printIterationStatement(node: ast.IterationStatement, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    // Omit printing 'IterationStatement' here, it's implied and only adds nesting.
    switch (node) {
        .do_while_statement => |x| try printDoWhileStatement(x, writer, indentation),
        .while_statement => |x| try printWhileStatement(x, writer, indentation),
        .for_statement => |x| try printForStatement(x, writer, indentation),
    }
}

pub fn printDoWhileStatement(node: ast.DoWhileStatement, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("DoWhileStatement", writer, indentation);
    try print("consequent:", writer, indentation + 1);
    try printStatement(node.consequent_statement.*, writer, indentation + 2);
    try print("test:", writer, indentation + 1);
    try printExpression(node.test_expression, writer, indentation + 2);
}

pub fn printWhileStatement(node: ast.WhileStatement, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("WhileStatement", writer, indentation);
    try print("test:", writer, indentation + 1);
    try printExpression(node.test_expression, writer, indentation + 2);
    try print("consequent:", writer, indentation + 1);
    try printStatement(node.consequent_statement.*, writer, indentation + 2);
}

pub fn printForStatement(node: ast.ForStatement, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("ForStatement", writer, indentation);
    if (node.initializer) |initializer| {
        try print("initializer:", writer, indentation + 1);
        switch (initializer) {
            .expression => |x| try printExpression(x, writer, indentation + 2),
            .variable_statement => |x| try printVariableStatement(x, writer, indentation + 2),
            .lexical_declaration => |x| try printLexicalDeclaration(x, writer, indentation + 2),
        }
    }
    if (node.test_expression) |test_expression| {
        try print("test:", writer, indentation + 1);
        try printExpression(test_expression, writer, indentation + 2);
    }
    if (node.increment_expression) |increment_expression| {
        try print("increment:", writer, indentation + 1);
        try printExpression(increment_expression, writer, indentation + 2);
    }
    try print("consequent:", writer, indentation + 1);
    try printStatement(node.consequent_statement.*, writer, indentation + 2);
}

pub fn printContinueStatement(node: ast.ContinueStatement, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("ContinueStatement", writer, indentation);
    if (node.label) |label| try print(label, writer, indentation + 1);
}

pub fn printBreakStatement(node: ast.BreakStatement, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("BreakStatement", writer, indentation);
    if (node.label) |label| try print(label, writer, indentation + 1);
}

pub fn printReturnStatement(node: ast.ReturnStatement, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("ReturnStatement", writer, indentation);
    if (node.expression) |expression| try printExpression(expression, writer, indentation + 1);
}

pub fn printThrowStatement(node: ast.ThrowStatement, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("ThrowStatement", writer, indentation);
    try printExpression(node.expression, writer, indentation + 1);
}

pub fn printTryStatement(node: ast.TryStatement, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("TryStatement", writer, indentation);
    try print("try:", writer, indentation + 1);
    try printBlock(node.try_block, writer, indentation + 2);
    if (node.catch_block) |catch_block| {
        try print("catch:", writer, indentation + 1);
        if (node.catch_parameter) |catch_parameter| {
            try print(catch_parameter, writer, indentation + 2);
        }
        try printBlock(catch_block, writer, indentation + 2);
    }
    if (node.finally_block) |finally_block| {
        try print("finally:", writer, indentation + 1);
        try printBlock(finally_block, writer, indentation + 2);
    }
}

pub fn printFormalParameters(node: ast.FormalParameters, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    // Omit printing 'FormalParameters' here, it's implied and only adds nesting.
    for (node.items) |item| {
        switch (item) {
            .formal_parameter => |x| try printFormalParameter(x, writer, indentation),
            .function_rest_parameter => |x| try printFunctionRestParameter(x, writer, indentation),
        }
    }
}

pub fn printFunctionRestParameter(node: ast.FunctionRestParameter, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("FunctionRestParameter", writer, indentation);
    try printBindingRestElement(node.binding_rest_element, writer, indentation + 1);
}

pub fn printFormalParameter(node: ast.FormalParameter, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("FormalParameter", writer, indentation);
    try printBindingElement(node.binding_element, writer, indentation + 1);
}

pub fn printFunctionDeclaration(node: ast.FunctionDeclaration, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("FunctionDeclaration", writer, indentation);
    try print("identifier:", writer, indentation + 1);
    try print(node.identifier, writer, indentation + 2);
    try print("formal_parameters:", writer, indentation + 1);
    try printFormalParameters(node.formal_parameters, writer, indentation + 2);
    try print("function_body:", writer, indentation + 1);
    try printFunctionBody(node.function_body, writer, indentation + 2);
}

pub fn printFunctionExpression(node: ast.FunctionExpression, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("FunctionExpression", writer, indentation);
    try print("identifier:", writer, indentation + 1);
    if (node.identifier) |identifier| try print(identifier, writer, indentation + 2);
    try print("formal_parameters:", writer, indentation + 1);
    try printFormalParameters(node.formal_parameters, writer, indentation + 2);
    try print("function_body:", writer, indentation + 1);
    try printFunctionBody(node.function_body, writer, indentation + 2);
}

pub fn printFunctionBody(node: ast.FunctionBody, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    // Omit printing 'FunctionBody' here, it's implied and only adds nesting.
    try printStatementList(node.statement_list, writer, indentation);
}

pub fn printArrowFunction(node: ast.ArrowFunction, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("ArrowFunction", writer, indentation);
    try print("formal_parameters:", writer, indentation + 1);
    try printFormalParameters(node.formal_parameters, writer, indentation + 2);
    try print("function_body:", writer, indentation + 1);
    try printFunctionBody(node.function_body, writer, indentation + 2);
}

pub fn printMethodDefinition(node: ast.MethodDefinition, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("MethodDefinition", writer, indentation);
    try print("type:", writer, indentation + 1);
    try print(@tagName(std.meta.activeTag(node.method)), writer, indentation + 2);
    try printPropertyName(node.property_name, writer, indentation + 1);
    switch (node.method) {
        .method, .get, .set => |x| try printFunctionExpression(x, writer, indentation + 1),
        .generator => |x| try printGeneratorExpression(x, writer, indentation + 1),
        .@"async" => |x| try printAsyncFunctionExpression(x, writer, indentation + 1),
        .async_generator => |x| try printAsyncGeneratorExpression(x, writer, indentation + 1),
    }
}

pub fn printGeneratorDeclaration(node: ast.GeneratorDeclaration, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("GeneratorDeclaration", writer, indentation);
    try print("identifier:", writer, indentation + 1);
    try print(node.identifier, writer, indentation + 2);
    try print("formal_parameters:", writer, indentation + 1);
    try printFormalParameters(node.formal_parameters, writer, indentation + 2);
    try print("function_body:", writer, indentation + 1);
    try printFunctionBody(node.function_body, writer, indentation + 2);
}

pub fn printGeneratorExpression(node: ast.GeneratorExpression, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("GeneratorExpression", writer, indentation);
    try print("identifier:", writer, indentation + 1);
    if (node.identifier) |identifier| try print(identifier, writer, indentation + 2);
    try print("formal_parameters:", writer, indentation + 1);
    try printFormalParameters(node.formal_parameters, writer, indentation + 2);
    try print("function_body:", writer, indentation + 1);
    try printFunctionBody(node.function_body, writer, indentation + 2);
}

pub fn printAsyncGeneratorDeclaration(node: ast.AsyncGeneratorDeclaration, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("AsyncGeneratorDeclaration", writer, indentation);
    try print("identifier:", writer, indentation + 1);
    if (node.identifier) |identifier| try print(identifier, writer, indentation + 2);
    try print("formal_parameters:", writer, indentation + 1);
    try printFormalParameters(node.formal_parameters, writer, indentation + 2);
    try print("function_body:", writer, indentation + 1);
    try printFunctionBody(node.function_body, writer, indentation + 2);
}

pub fn printAsyncGeneratorExpression(node: ast.AsyncGeneratorExpression, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("AsyncGeneratorExpression", writer, indentation);
    try print("identifier:", writer, indentation + 1);
    if (node.identifier) |identifier| try print(identifier, writer, indentation + 2);
    try print("formal_parameters:", writer, indentation + 1);
    try printFormalParameters(node.formal_parameters, writer, indentation + 2);
    try print("function_body:", writer, indentation + 1);
    try printFunctionBody(node.function_body, writer, indentation + 2);
}

pub fn printClassDeclaration(node: ast.ClassDeclaration, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("ClassDeclaration", writer, indentation);
    try print("identifier:", writer, indentation + 1);
    if (node.identifier) |identifier| try print(identifier, writer, indentation + 2);
    try printClassTail(node.class_tail, writer, indentation + 1);
}

pub fn printClassExpression(node: ast.ClassExpression, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("ClassExpression", writer, indentation);
    try print("identifier:", writer, indentation + 1);
    if (node.identifier) |identifier| try print(identifier, writer, indentation + 2);
    try printClassTail(node.class_tail, writer, indentation + 1);
}

pub fn printClassTail(node: ast.ClassTail, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    // Omit printing 'ClassTail' here, it's implied and only adds nesting.
    if (node.class_heritage) |class_heritage| {
        try print("extends:", writer, indentation);
        try printExpression(class_heritage.*, writer, indentation + 1);
    }
    try printClassBody(node.class_body, writer, indentation);
}

pub fn printClassBody(node: ast.ClassBody, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    // Omit printing 'ClassBody' here, it's implied and only adds nesting.
    try printClassElementList(node.class_element_list, writer, indentation);
}

pub fn printClassElementList(node: ast.ClassElementList, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    // Omit printing 'ClassElementList' here, it's implied and only adds nesting.
    for (node.items) |item| {
        try printClassElement(item, writer, indentation);
    }
}

pub fn printClassElement(node: ast.ClassElement, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    // Omit printing 'ClassElement' here, it's implied and only adds nesting.
    switch (node) {
        .empty_statement => try print("empty", writer, indentation),
        .method_definition => |x| try printMethodDefinition(x, writer, indentation),
        .static_method_definition => |x| try printMethodDefinition(x, writer, indentation),
        .field_definition => |x| try printFieldDefinition(x, writer, indentation),
        .static_field_definition => |x| try printFieldDefinition(x, writer, indentation),
        .class_static_block => |x| try printClassStaticBlock(x, writer, indentation),
    }
}

pub fn printFieldDefinition(node: ast.FieldDefinition, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("FieldDefinition", writer, indentation);
    try printPropertyName(node.property_name, writer, indentation + 1);
    if (node.initializer) |initializer| try printExpression(initializer, writer, indentation + 1);
}

pub fn printClassStaticBlock(node: ast.ClassStaticBlock, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("ClassStaticBlock", writer, indentation);
    try printStatementList(node.statement_list, writer, indentation + 1);
}

pub fn printAsyncFunctionDeclaration(node: ast.AsyncFunctionDeclaration, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("AsyncFunctionDeclaration", writer, indentation);
    try print("identifier:", writer, indentation + 1);
    if (node.identifier) |identifier| try print(identifier, writer, indentation + 2);
    try print("formal_parameters:", writer, indentation + 1);
    try printFormalParameters(node.formal_parameters, writer, indentation + 2);
    try print("function_body:", writer, indentation + 1);
    try printFunctionBody(node.function_body, writer, indentation + 2);
}

pub fn printAsyncFunctionExpression(node: ast.AsyncFunctionExpression, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("AsyncFunctionExpression", writer, indentation);
    try print("identifier:", writer, indentation + 1);
    if (node.identifier) |identifier| try print(identifier, writer, indentation + 2);
    try print("formal_parameters:", writer, indentation + 1);
    try printFormalParameters(node.formal_parameters, writer, indentation + 2);
    try print("function_body:", writer, indentation + 1);
    try printFunctionBody(node.function_body, writer, indentation + 2);
}

pub fn printAsyncArrowFunction(node: ast.AsyncArrowFunction, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("AsyncArrowFunction", writer, indentation);
    try print("formal_parameters:", writer, indentation + 1);
    try printFormalParameters(node.formal_parameters, writer, indentation + 2);
    try print("function_body:", writer, indentation + 1);
    try printFunctionBody(node.function_body, writer, indentation + 2);
}

pub fn printScript(node: ast.Script, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("Script", writer, indentation);
    try print("strict:", writer, indentation + 1);
    try print(if (node.isStrict()) "true" else "false", writer, indentation + 2);
    try printStatementList(node.statement_list, writer, indentation + 1);
}

pub fn printModule(node: ast.Module, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("Module", writer, indentation);
    try printModuleItemList(node.module_item_list, writer, indentation + 1);
}

pub fn printModuleItemList(node: ast.ModuleItemList, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    // Omit printing 'ModuleItemList' here, it's implied and only adds nesting.
    for (node.items) |item| {
        try printModuleItem(item, writer, indentation);
    }
}

pub fn printModuleItem(node: ast.ModuleItem, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    // Omit printing 'ModuleItem' here, it's implied and only adds nesting.
    switch (node) {
        .import_declaration => |x| try printImportDeclaration(x, writer, indentation),
        .export_declaration => |x| try printExportDeclaration(x, writer, indentation),
        .statement_list_item => |x| try printStatementListItem(x, writer, indentation),
    }
}

pub fn printModuleExportName(node: ast.ModuleExportName, writer: anytype) @TypeOf(writer).Error!void {
    // NOTE: These are always printed inline, so no newline or indentation are added.
    switch (node) {
        .identifier => |identifier| try writer.writeAll(identifier),
        .string_literal => |string_literal| try writer.writeAll(string_literal.text),
    }
}

pub fn printImportDeclaration(node: ast.ImportDeclaration, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("ImportDeclaration", writer, indentation);
    if (node.import_clause) |import_clause| try printImportClause(import_clause, writer, indentation + 1);
    try print(node.module_specifier.text, writer, indentation + 1);
}

pub fn printImportClause(node: ast.ImportClause, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("ImportClause", writer, indentation);
    switch (node) {
        .imported_default_binding => |imported_default_binding| {
            try print(imported_default_binding.binding_identifier, writer, indentation + 1);
        },
    }
}

pub fn printExportDeclaration(node: ast.ExportDeclaration, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("ExportDeclaration", writer, indentation);
    switch (node) {
        .default_hoistable_declaration,
        .default_class_declaration,
        .default_expression,
        => try print("default", writer, indentation + 1),
        else => {},
    }
    switch (node) {
        .export_from => |export_from| {
            try printExportFromClause(export_from.export_from_clause, writer, indentation);
            try print("from", writer, indentation);
            try print(export_from.module_specifier.text, writer, indentation + 1);
        },
        .named_exports => |x| try printNamedExports(x, writer, indentation + 1),
        .declaration => |x| try printDeclaration(x.*, writer, indentation + 1),
        .variable_statement => |x| try printVariableStatement(x, writer, indentation + 1),
        .default_hoistable_declaration => |x| try printHoistableDeclaration(x, writer, indentation + 1),
        .default_class_declaration => |x| try printClassDeclaration(x, writer, indentation + 1),
        .default_expression => |x| try printExpression(x, writer, indentation + 1),
    }
}

pub fn printExportFromClause(node: ast.ExportFromClause, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    // Omit printing 'ExportFromClause' here, it's implied and only adds nesting.
    switch (node) {
        .star => try print("*", writer, indentation),
        .star_as => |module_export_name| {
            try printIndentation(writer, indentation);
            try writer.writeAll("* as ");
            try printModuleExportName(module_export_name, writer);
            try writer.writeAll("\n");
        },
        .named_exports => |named_exports| try printNamedExports(named_exports, writer, indentation),
    }
}

pub fn printNamedExports(node: ast.NamedExports, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    try print("NamedExports", writer, indentation);
    try printExportsList(node.exports_list, writer, indentation + 1);
}

pub fn printExportsList(node: ast.ExportsList, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    // Omit printing 'ExportsList' here, it's implied and only adds nesting.
    for (node.items) |item| {
        try printExportSpecifier(item, writer, indentation);
    }
}

pub fn printExportSpecifier(node: ast.ExportSpecifier, writer: anytype, indentation: usize) @TypeOf(writer).Error!void {
    // Omit printing 'ExportSpecifier' here, it's implied and only adds nesting.
    try printIndentation(writer, indentation);
    try printModuleExportName(node.name, writer);
    if (node.alias) |alias| {
        try writer.writeAll(" as ");
        try printModuleExportName(alias, writer);
    }
    try writer.writeAll("\n");
}
