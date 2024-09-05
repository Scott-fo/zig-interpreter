const std = @import("std");

pub const TokenType = enum {
    ILLEGAL,
    EOF,

    // Punctuation
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

    // Ident
    FUNCTION,
    LET,
    IDENT,
    INT,
    IF,
    ELSE,
    RETURN,

    TRUE,
    FALSE,
};

pub const Token = struct {
    type: TokenType,
    literal: []const u8,
};

pub const keywords = std.StaticStringMap(TokenType).initComptime(.{
    .{ "let", .LET },
    .{ "fn", .FUNCTION },
    .{ "true", .TRUE },
    .{ "false", .FALSE },
    .{ "if", .IF },
    .{ "else", .ELSE },
    .{ "return", .RETURN },
});

pub fn lookup_ident(ident: []const u8) TokenType {
    return keywords.get(ident) orelse .IDENT;
}
