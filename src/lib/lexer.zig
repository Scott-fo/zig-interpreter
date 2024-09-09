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

        l.readChar();
        return l;
    }

    pub fn nextToken(self: *Lexer) Token {
        self.skipWhitespace();

        const tok: Token = switch (self.ch) {
            '=' => blk: {
                if (self.peekChar() == '=') {
                    self.readChar();
                    break :blk .EQ;
                } else {
                    break :blk .ASSIGN;
                }
            },
            '!' => blk: {
                if (self.peekChar() == '=') {
                    self.readChar();
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
                const ident = self.readIdentifier();
                if (Token.keyword(ident)) |token| {
                    return token;
                }

                return .{ .IDENT = ident };
            },
            '0'...'9' => {
                const int = self.readNumber();
                return .{ .INT = int };
            },
            0 => .EOF,
            else => .ILLEGAL,
        };

        self.readChar();
        return tok;
    }

    fn readChar(self: *Lexer) void {
        if (self.read_position >= self.input.len) {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn readIdentifier(self: *Lexer) []const u8 {
        const p = self.position;

        while (isLetter(self.ch)) {
            self.readChar();
        }

        return self.input[p..self.position];
    }

    fn skipWhitespace(self: *Lexer) void {
        while (std.ascii.isWhitespace(self.ch)) {
            self.readChar();
        }
    }

    fn readNumber(self: *Lexer) []const u8 {
        const p = self.position;

        while (isDigit(self.ch)) {
            self.readChar();
        }

        return self.input[p..self.position];
    }

    fn peekChar(self: *Lexer) u8 {
        if (self.read_position >= self.input.len) {
            return 0;
        }

        return self.input[self.read_position];
    }
};

fn isLetter(ch: u8) bool {
    return std.ascii.isAlphabetic(ch) or ch == '_';
}

fn isDigit(ch: u8) bool {
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
        const tok = lexer.nextToken();
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
        const tok = lexer.nextToken();
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
        const tok = lexer.nextToken();
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
        const tok = lexer.nextToken();
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
        const tok = lexer.nextToken();
        try std.testing.expectEqualDeep(t, tok);
    }
}
