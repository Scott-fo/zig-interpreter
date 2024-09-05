const std = @import("std");
const token = @import("token.zig");
const TokenType = token.TokenType;

const Lexer = struct {
    input: []const u8,
    position: usize,
    read_position: usize,
    ch: u8,

    fn init(i: []const u8) Lexer {
        var l = Lexer{
            .input = i,
            .position = 0,
            .read_position = 0,
            .ch = 0,
        };

        l.read_char();
        return l;
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

    fn next_token(self: *Lexer) token.Token {
        self.skip_whitespace();

        const tok = switch (self.ch) {
            '=' => blk: {
                if (self.peek_char() == '=') {
                    self.read_char();
                    break :blk new_token(TokenType.EQ, "==");
                } else {
                    break :blk new_token(TokenType.ASSIGN, "=");
                }
            },
            ';' => new_token(TokenType.SEMICOLON, ";"),
            '(' => new_token(TokenType.LPAREN, "("),
            ')' => new_token(TokenType.RPAREN, ")"),
            ',' => new_token(TokenType.COMMA, ","),
            '+' => new_token(TokenType.PLUS, "+"),
            '{' => new_token(TokenType.LBRACE, "{"),
            '}' => new_token(TokenType.RBRACE, "}"),
            '!' => blk: {
                if (self.peek_char() == '=') {
                    self.read_char();
                    break :blk new_token(TokenType.NOT_EQ, "!=");
                } else {
                    break :blk new_token(TokenType.BANG, "!");
                }
            },
            '-' => new_token(TokenType.MINUS, "-"),
            '/' => new_token(TokenType.SLASH, "/"),
            '*' => new_token(TokenType.ASTERISK, "*"),
            '<' => new_token(TokenType.LT, "<"),
            '>' => new_token(TokenType.GT, ">"),
            0 => new_token(TokenType.EOF, ""),
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
        .{ .expectedType = TokenType.ASSIGN, .expectedLiteral = "=" },
        .{ .expectedType = TokenType.PLUS, .expectedLiteral = "+" },
        .{ .expectedType = TokenType.LPAREN, .expectedLiteral = "(" },
        .{ .expectedType = TokenType.RPAREN, .expectedLiteral = ")" },
        .{ .expectedType = TokenType.LBRACE, .expectedLiteral = "{" },
        .{ .expectedType = TokenType.RBRACE, .expectedLiteral = "}" },
        .{ .expectedType = TokenType.COMMA, .expectedLiteral = "," },
        .{ .expectedType = TokenType.SEMICOLON, .expectedLiteral = ";" },
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
        .{ .expectedType = TokenType.LET, .expectedLiteral = "let" },
        .{ .expectedType = TokenType.IDENT, .expectedLiteral = "five" },
        .{ .expectedType = TokenType.ASSIGN, .expectedLiteral = "=" },
        .{ .expectedType = TokenType.INT, .expectedLiteral = "5" },
        .{ .expectedType = TokenType.SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = TokenType.LET, .expectedLiteral = "let" },
        .{ .expectedType = TokenType.IDENT, .expectedLiteral = "ten" },
        .{ .expectedType = TokenType.ASSIGN, .expectedLiteral = "=" },
        .{ .expectedType = TokenType.INT, .expectedLiteral = "10" },
        .{ .expectedType = TokenType.SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = TokenType.LET, .expectedLiteral = "let" },
        .{ .expectedType = TokenType.IDENT, .expectedLiteral = "add" },
        .{ .expectedType = TokenType.ASSIGN, .expectedLiteral = "=" },
        .{ .expectedType = TokenType.FUNCTION, .expectedLiteral = "fn" },
        .{ .expectedType = TokenType.LPAREN, .expectedLiteral = "(" },
        .{ .expectedType = TokenType.IDENT, .expectedLiteral = "x" },
        .{ .expectedType = TokenType.COMMA, .expectedLiteral = "," },
        .{ .expectedType = TokenType.IDENT, .expectedLiteral = "y" },
        .{ .expectedType = TokenType.RPAREN, .expectedLiteral = ")" },
        .{ .expectedType = TokenType.LBRACE, .expectedLiteral = "{" },
        .{ .expectedType = TokenType.IDENT, .expectedLiteral = "x" },
        .{ .expectedType = TokenType.PLUS, .expectedLiteral = "+" },
        .{ .expectedType = TokenType.IDENT, .expectedLiteral = "y" },
        .{ .expectedType = TokenType.SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = TokenType.RBRACE, .expectedLiteral = "}" },
        .{ .expectedType = TokenType.SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = TokenType.LET, .expectedLiteral = "let" },
        .{ .expectedType = TokenType.IDENT, .expectedLiteral = "result" },
        .{ .expectedType = TokenType.ASSIGN, .expectedLiteral = "=" },
        .{ .expectedType = TokenType.IDENT, .expectedLiteral = "add" },
        .{ .expectedType = TokenType.LPAREN, .expectedLiteral = "(" },
        .{ .expectedType = TokenType.IDENT, .expectedLiteral = "five" },
        .{ .expectedType = TokenType.COMMA, .expectedLiteral = "," },
        .{ .expectedType = TokenType.IDENT, .expectedLiteral = "ten" },
        .{ .expectedType = TokenType.RPAREN, .expectedLiteral = ")" },
        .{ .expectedType = TokenType.SEMICOLON, .expectedLiteral = ";" },
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
        .{ .expectedType = TokenType.BANG, .expectedLiteral = "!" },
        .{ .expectedType = TokenType.MINUS, .expectedLiteral = "-" },
        .{ .expectedType = TokenType.SLASH, .expectedLiteral = "/" },
        .{ .expectedType = TokenType.ASTERISK, .expectedLiteral = "*" },
        .{ .expectedType = TokenType.INT, .expectedLiteral = "5" },
        .{ .expectedType = TokenType.SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = TokenType.INT, .expectedLiteral = "5" },
        .{ .expectedType = TokenType.LT, .expectedLiteral = "<" },
        .{ .expectedType = TokenType.INT, .expectedLiteral = "10" },
        .{ .expectedType = TokenType.GT, .expectedLiteral = ">" },
        .{ .expectedType = TokenType.INT, .expectedLiteral = "5" },
        .{ .expectedType = TokenType.SEMICOLON, .expectedLiteral = ";" },
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
        .{ .expectedType = TokenType.IF, .expectedLiteral = "if" },
        .{ .expectedType = TokenType.LPAREN, .expectedLiteral = "(" },
        .{ .expectedType = TokenType.INT, .expectedLiteral = "5" },
        .{ .expectedType = TokenType.LT, .expectedLiteral = "<" },
        .{ .expectedType = TokenType.INT, .expectedLiteral = "10" },
        .{ .expectedType = TokenType.RPAREN, .expectedLiteral = ")" },
        .{ .expectedType = TokenType.LBRACE, .expectedLiteral = "{" },
        .{ .expectedType = TokenType.RETURN, .expectedLiteral = "return" },
        .{ .expectedType = TokenType.TRUE, .expectedLiteral = "true" },
        .{ .expectedType = TokenType.SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = TokenType.RBRACE, .expectedLiteral = "}" },
        .{ .expectedType = TokenType.ELSE, .expectedLiteral = "else" },
        .{ .expectedType = TokenType.LBRACE, .expectedLiteral = "{" },
        .{ .expectedType = TokenType.RETURN, .expectedLiteral = "return" },
        .{ .expectedType = TokenType.FALSE, .expectedLiteral = "false" },
        .{ .expectedType = TokenType.SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = TokenType.RBRACE, .expectedLiteral = "}" },
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
        .{ .expectedType = TokenType.INT, .expectedLiteral = "10" },
        .{ .expectedType = TokenType.EQ, .expectedLiteral = "==" },
        .{ .expectedType = TokenType.INT, .expectedLiteral = "10" },
        .{ .expectedType = TokenType.SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = TokenType.INT, .expectedLiteral = "10" },
        .{ .expectedType = TokenType.NOT_EQ, .expectedLiteral = "!=" },
        .{ .expectedType = TokenType.INT, .expectedLiteral = "9" },
        .{ .expectedType = TokenType.SEMICOLON, .expectedLiteral = ";" },
    };

    var lexer = Lexer.init(input);

    for (tests) |t| {
        const tok = lexer.next_token();
        try std.testing.expectEqual(t.expectedType, tok.type);
        try std.testing.expectEqualStrings(t.expectedLiteral, tok.literal);
    }
}
