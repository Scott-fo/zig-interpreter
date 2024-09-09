const std = @import("std");
const Token = @import("token.zig").Token;

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

    pub fn next_token(self: *Lexer) Token {
        self.skip_whitespace();

        const tok: Token = switch (self.ch) {
            '=' => blk: {
                if (self.peek_char() == '=') {
                    self.read_char();
                    break :blk .EQ;
                } else {
                    break :blk .ASSIGN;
                }
            },
            '!' => blk: {
                if (self.peek_char() == '=') {
                    self.read_char();
                    break :blk .NOT_EQ;
                } else {
                    break :blk .BANG;
                }
            },
            ';' => .SEMICOLON,
            '(' => .LPAREN,
            ')' => .RPAREN,
            ',' => .COMMA,
            '+' => .PLUS,
            '{' => .LBRACE,
            '}' => .RBRACE,
            '-' => .MINUS,
            '/' => .SLASH,
            '*' => .ASTERISK,
            '<' => .LT,
            '>' => .GT,
            'a'...'z', 'A'...'Z', '_' => {
                const ident = self.read_identifier();
                if (Token.keyword(ident)) |token| {
                    return token;
                }

                return .{ .IDENT = ident };
            },
            '0'...'9' => {
                const int = self.read_number();
                return .{ .INT = int };
            },
            0 => .EOF,
            else => .ILLEGAL,
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

    fn read_identifier(self: *Lexer) []const u8 {
        const p = self.position;

        while (is_letter(self.ch)) {
            self.read_char();
        }

        return self.input[p..self.position];
    }

    fn skip_whitespace(self: *Lexer) void {
        while (std.ascii.isWhitespace(self.ch)) {
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
    return std.ascii.isAlphabetic(ch) or ch == '_';
}

fn is_digit(ch: u8) bool {
    return std.ascii.isDigit(ch);
}

test "next token" {
    const input = "=+(){},;";

    const tokens = [_]Token{
        .ASSIGN,
        .PLUS,
        .LPAREN,
        .RPAREN,
        .LBRACE,
        .RBRACE,
        .COMMA,
        .SEMICOLON,
    };

    var lexer = Lexer.init(input);

    for (tokens) |t| {
        const tok = lexer.next_token();
        try std.testing.expectEqualDeep(t, tok);
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

    const tokens = [_]Token{
        .LET,
        .{ .IDENT = "five" },
        .ASSIGN,
        .{ .INT = "5" },
        .SEMICOLON,
        .LET,
        .{ .IDENT = "ten" },
        .ASSIGN,
        .{ .INT = "10" },
        .SEMICOLON,
        .LET,
        .{ .IDENT = "add" },
        .ASSIGN,
        .FUNCTION,
        .LPAREN,
        .{ .IDENT = "x" },
        .COMMA,
        .{ .IDENT = "y" },
        .RPAREN,
        .LBRACE,
        .{ .IDENT = "x" },
        .PLUS,
        .{ .IDENT = "y" },
        .SEMICOLON,
        .RBRACE,
        .SEMICOLON,
        .LET,
        .{ .IDENT = "result" },
        .ASSIGN,
        .{ .IDENT = "add" },
        .LPAREN,
        .{ .IDENT = "five" },
        .COMMA,
        .{ .IDENT = "ten" },
        .RPAREN,
        .SEMICOLON,
    };

    var lexer = Lexer.init(input);

    for (tokens) |t| {
        const tok = lexer.next_token();
        try std.testing.expectEqualDeep(t, tok);
    }
}

test "test punctuation" {
    const input =
        \\!-/*5;
        \\5 < 10 > 5;
    ;

    const tokens = [_]Token{
        .BANG,
        .MINUS,
        .SLASH,
        .ASTERISK,
        .{ .INT = "5" },
        .SEMICOLON,
        .{ .INT = "5" },
        .LT,
        .{ .INT = "10" },
        .GT,
        .{ .INT = "5" },
        .SEMICOLON,
    };

    var lexer = Lexer.init(input);

    for (tokens) |t| {
        const tok = lexer.next_token();
        try std.testing.expectEqualDeep(t, tok);
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

    const tokens = [_]Token{
        .IF,
        .LPAREN,
        .{ .INT = "5" },
        .LT,
        .{ .INT = "10" },
        .RPAREN,
        .LBRACE,
        .RETURN,
        .TRUE,
        .SEMICOLON,
        .RBRACE,
        .ELSE,
        .LBRACE,
        .RETURN,
        .FALSE,
        .SEMICOLON,
        .RBRACE,
    };

    var lexer = Lexer.init(input);

    for (tokens) |t| {
        const tok = lexer.next_token();
        try std.testing.expectEqualDeep(t, tok);
    }
}

test "equality" {
    const input =
        \\10 == 10;
        \\10 != 9;
    ;

    const tokens = [_]Token{
        .{ .INT = "10" },
        .EQ,
        .{ .INT = "10" },
        .SEMICOLON,
        .{ .INT = "10" },
        .NOT_EQ,
        .{ .INT = "9" },
        .SEMICOLON,
    };

    var lexer = Lexer.init(input);

    for (tokens) |t| {
        const tok = lexer.next_token();
        try std.testing.expectEqualDeep(t, tok);
    }
}
