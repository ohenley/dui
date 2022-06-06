with widget;
with widget.text;
with widget.image;
with widget.button;
with graphic;

with Ada.Strings.Unbounded;
with Ada.Finalization;
with Ada.Text_IO;

with System.Storage_Elements;

package namespaces is
    package w renames widget;
    package wt renames widget.text;
    package wi renames widget.image;
    package wb renames widget.button;
    package g  renames graphic;

    package su renames Ada.Strings.Unbounded;
    package af renames Ada.Finalization;
    package aio renames Ada.Text_IO;

    package sse renames System.Storage_Elements;
end namespaces;