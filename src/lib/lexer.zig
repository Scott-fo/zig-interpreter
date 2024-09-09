const std = @import("std");
const token = @import("token.zig");
const TokenType = token.TokenType;

pub const Lexer = struct {
    input: []const u8,
    position: usize = 0,
    read_position: usize = 0,
    ch: u8 = 0,

    pub fn init(i: []const u8) Lexer {
        var l = Lexer{
            .input = i,
        };

        l.read_char();
        return l;
    }

    pub fn next_token(self: *Lexer) token.Token {
        self.skip_whitespace();

        const tok = switch (self.ch) {
            '=' => blk: {
                if (self.peek_char() == '=') {
                    self.read_char();
                    break :blk new_token(.EQ, "==");
                } else {
                    break :blk new_token(.ASSIGN, "=");
                }
            },
            '!' => blk: {
                if (self.peek_char() == '=') {
                    self.read_char();
                    break :blk new_token(.NOT_EQ, "!=");
                } else {
                    break :blk new_token(.BANG, "!");
                }
            },
            ';' => new_token(.SEMICOLON, ";"),
            '(' => new_token(.LPAREN, "("),
            ')' => new_token(.RPAREN, ")"),
            ',' => new_token(.COMMA, ","),
            '+' => new_token(.PLUS, "+"),
            '{' => new_token(.LBRACE, "{"),
            '}' => new_token(.RBRACE, "}"),
            '-' => new_token(.MINUS, "-"),
            '/' => new_token(.SLASH, "/"),
            '*' => new_token(.ASTERISK, "*"),
            '<' => new_token(.LT, "<"),
            '>' => new_token(.GT, ">"),
            0 => new_token(.EOF, ""),
            else => {
                if (is_letter(self.ch)) {
                    const literal = self.read_identifier();
                    return new_token(token.lookup_ident(literal), literal);
                } else if (is_digit(self.ch)) {
                    return new_token(TokenType.INT, self.read_number());
                } else {
                    return new_token(TokenType.ILLEGAL, &[_]u8{self.ch});
                }
            },
        };

        self.read_char();
        return tok;
    }

    fn read_char(self: *Lexer) void {
        if (self.read_position >= self.input.len) {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn new_token(token_type: token.TokenType, ch: []const u8) token.Token {
        return .{
            .type = token_type,
            .literal = ch,
        };
    }

    fn read_identifier(self: *Lexer) []const u8 {
        const p = self.position;

        while (is_letter(self.ch)) {
            self.read_char();
        }

        return self.input[p..self.position];
    }

    fn skip_whitespace(self: *Lexer) void {
        while (self.ch == ' ' or self.ch == '\n' or self.ch == '\r') {
            self.read_char();
        }
    }

    fn read_number(self: *Lexer) []const u8 {
        const p = self.position;

        while (is_digit(self.ch)) {
            self.read_char();
        }

        return self.input[p..self.position];
    }

    fn peek_char(self: *Lexer) u8 {
        if (self.read_position >= self.input.len) {
            return 0;
        }

        return self.input[self.read_position];
    }
};

fn is_letter(ch: u8) bool {
    return ('a' <= ch and ch <= 'z') or ('A' <= ch and ch <= 'Z') or ch == '_';
}

fn is_digit(ch: u8) bool {
    return '0' <= ch and ch <= '9';
}

test "next token" {
    const input = "=+(){},;";
    const tests = [_]struct {
        expectedType: TokenType,
        expectedLiteral: []const u8,
    }{
        .{ .expectedType = .ASSIGN, .expectedLiteral = "=" },
        .{ .expectedType = .PLUS, .expectedLiteral = "+" },
        .{ .expectedType = .LPAREN, .expectedLiteral = "(" },
        .{ .expectedType = .RPAREN, .expectedLiteral = ")" },
        .{ .expectedType = .LBRACE, .expectedLiteral = "{" },
        .{ .expectedType = .RBRACE, .expectedLiteral = "}" },
        .{ .expectedType = .COMMA, .expectedLiteral = "," },
        .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
    };

    var lexer = Lexer.init(input);

    for (tests) |t| {
        const tok = lexer.next_token();
        try std.testing.expectEqual(t.expectedType, tok.type);
        try std.testing.expectEqualStrings(t.expectedLiteral, tok.literal);
    }
}

test "let statement" {
    const input =
        \\let five = 5;
        \\let ten = 10;
        \\let add = fn(x, y) {
        \\ x + y;
        \\};
        \\
        \\let result = add(five, ten);
    ;

    const tests = [_]struct {
        expectedType: TokenType,
        expectedLiteral: []const u8,
    }{
        .{ .expectedType = .LET, .expectedLiteral = "let" },
        .{ .expectedType = .IDENT, .expectedLiteral = "five" },
        .{ .expectedType = .ASSIGN, .expectedLiteral = "=" },
        .{ .expectedType = .INT, .expectedLiteral = "5" },
        .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = .LET, .expectedLiteral = "let" },
        .{ .expectedType = .IDENT, .expectedLiteral = "ten" },
        .{ .expectedType = .ASSIGN, .expectedLiteral = "=" },
        .{ .expectedType = .INT, .expectedLiteral = "10" },
        .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = .LET, .expectedLiteral = "let" },
        .{ .expectedType = .IDENT, .expectedLiteral = "add" },
        .{ .expectedType = .ASSIGN, .expectedLiteral = "=" },
        .{ .expectedType = .FUNCTION, .expectedLiteral = "fn" },
        .{ .expectedType = .LPAREN, .expectedLiteral = "(" },
        .{ .expectedType = .IDENT, .expectedLiteral = "x" },
        .{ .expectedType = .COMMA, .expectedLiteral = "," },
        .{ .expectedType = .IDENT, .expectedLiteral = "y" },
        .{ .expectedType = .RPAREN, .expectedLiteral = ")" },
        .{ .expectedType = .LBRACE, .expectedLiteral = "{" },
        .{ .expectedType = .IDENT, .expectedLiteral = "x" },
        .{ .expectedType = .PLUS, .expectedLiteral = "+" },
        .{ .expectedType = .IDENT, .expectedLiteral = "y" },
        .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = .RBRACE, .expectedLiteral = "}" },
        .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = .LET, .expectedLiteral = "let" },
        .{ .expectedType = .IDENT, .expectedLiteral = "result" },
        .{ .expectedType = .ASSIGN, .expectedLiteral = "=" },
        .{ .expectedType = .IDENT, .expectedLiteral = "add" },
        .{ .expectedType = .LPAREN, .expectedLiteral = "(" },
        .{ .expectedType = .IDENT, .expectedLiteral = "five" },
        .{ .expectedType = .COMMA, .expectedLiteral = "," },
        .{ .expectedType = .IDENT, .expectedLiteral = "ten" },
        .{ .expectedType = .RPAREN, .expectedLiteral = ")" },
        .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
    };

    var lexer = Lexer.init(input);

    for (tests) |t| {
        const tok = lexer.next_token();
        try std.testing.expectEqual(t.expectedType, tok.type);
        try std.testing.expectEqualStrings(t.expectedLiteral, tok.literal);
    }
}

test "test punctuation" {
    const input =
        \\!-/*5;
        \\5 < 10 > 5;
    ;

    const tests = [_]struct {
        expectedType: TokenType,
        expectedLiteral: []const u8,
    }{
        .{ .expectedType = .BANG, .expectedLiteral = "!" },
        .{ .expectedType = .MINUS, .expectedLiteral = "-" },
        .{ .expectedType = .SLASH, .expectedLiteral = "/" },
        .{ .expectedType = .ASTERISK, .expectedLiteral = "*" },
        .{ .expectedType = .INT, .expectedLiteral = "5" },
        .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = .INT, .expectedLiteral = "5" },
        .{ .expectedType = .LT, .expectedLiteral = "<" },
        .{ .expectedType = .INT, .expectedLiteral = "10" },
        .{ .expectedType = .GT, .expectedLiteral = ">" },
        .{ .expectedType = .INT, .expectedLiteral = "5" },
        .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
    };

    var lexer = Lexer.init(input);

    for (tests) |t| {
        const tok = lexer.next_token();
        try std.testing.expectEqual(t.expectedType, tok.type);
        try std.testing.expectEqualStrings(t.expectedLiteral, tok.literal);
    }
}

test "keywords" {
    const input =
        \\if(5 < 10) {
        \\  return true;
        \\} else {
        \\  return false;
        \\}
    ;

    const tests = [_]struct {
        expectedType: TokenType,
        expectedLiteral: []const u8,
    }{
        .{ .expectedType = .IF, .expectedLiteral = "if" },
        .{ .expectedType = .LPAREN, .expectedLiteral = "(" },
        .{ .expectedType = .INT, .expectedLiteral = "5" },
        .{ .expectedType = .LT, .expectedLiteral = "<" },
        .{ .expectedType = .INT, .expectedLiteral = "10" },
        .{ .expectedType = .RPAREN, .expectedLiteral = ")" },
        .{ .expectedType = .LBRACE, .expectedLiteral = "{" },
        .{ .expectedType = .RETURN, .expectedLiteral = "return" },
        .{ .expectedType = .TRUE, .expectedLiteral = "true" },
        .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = .RBRACE, .expectedLiteral = "}" },
        .{ .expectedType = .ELSE, .expectedLiteral = "else" },
        .{ .expectedType = .LBRACE, .expectedLiteral = "{" },
        .{ .expectedType = .RETURN, .expectedLiteral = "return" },
        .{ .expectedType = .FALSE, .expectedLiteral = "false" },
        .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = .RBRACE, .expectedLiteral = "}" },
    };

    var lexer = Lexer.init(input);

    for (tests) |t| {
        const tok = lexer.next_token();
        try std.testing.expectEqual(t.expectedType, tok.type);
        try std.testing.expectEqualStrings(t.expectedLiteral, tok.literal);
    }
}

test "equality" {
    const input =
        \\10 == 10;
        \\10 != 9;
    ;

    const tests = [_]struct {
        expectedType: TokenType,
        expectedLiteral: []const u8,
    }{
        .{ .expectedType = .INT, .expectedLiteral = "10" },
        .{ .expectedType = .EQ, .expectedLiteral = "==" },
        .{ .expectedType = .INT, .expectedLiteral = "10" },
        .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = .INT, .expectedLiteral = "10" },
        .{ .expectedType = .NOT_EQ, .expectedLiteral = "!=" },
        .{ .expectedType = .INT, .expectedLiteral = "9" },
        .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
    };

    var lexer = Lexer.init(input);

    for (tests) |t| {
        const tok = lexer.next_token();
        try std.testing.expectEqual(t.expectedType, tok.type);
        try std.testing.expectEqualStrings(t.expectedLiteral, tok.literal);
    }
}
