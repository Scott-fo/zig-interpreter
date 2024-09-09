const std = @import("std");

pub const Token = union(enum) {
    IDENT: []const u8,
    INT: []const u8,

    ILLEGAL,
    EOF,

    ASSIGN,
    PLUS,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    BANG,
    MINUS,
    SLASH,
    ASTERISK,
    LT,
    GT,
    EQ,
    NOT_EQ,

    FUNCTION,
    LET,
    IF,
    ELSE,
    RETURN,

    TRUE,
    FALSE,

    pub fn keyword(ident: []const u8) ?Token {
        const keywords = std.StaticStringMap(Token).initComptime(.{
            .{ "let", .LET },
            .{ "fn", .FUNCTION },
            .{ "true", .TRUE },
            .{ "false", .FALSE },
            .{ "if", .IF },
            .{ "else", .ELSE },
            .{ "return", .RETURN },
        });

        return keywords.get(ident);
    }

    pub fn toLiteral(self: Token) []const u8 {
        return switch (self) {
            .ASSIGN => "=",
            .PLUS => "+",
            .COMMA => ",",
            .SEMICOLON => ";",
            .LPAREN => "(",
            .RPAREN => ")",
            .LBRACE => "{",
            .RBRACE => "}",
            .BANG => "!",
            .MINUS => "-",
            .SLASH => "/",
            .ASTERISK => "*",
            .LT => "<",
            .GT => ">",
            .EQ => "==",
            .NOT_EQ => "!=",
            .FUNCTION => "fn",
            .LET => "let",
            .IF => "if",
            .ELSE => "else",
            .RETURN => "return",
            .TRUE => "true",
            .FALSE => "false",
            .EOF => "",
            else => unreachable,
        };
    }
};
