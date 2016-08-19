module Html = Dom_html

let escapeHtml fmt str =
    String.iter (fun chr -> Format.fprintf fmt "%s" @@ match chr with
        | '<' -> "&lt;"
        | '>' -> "&gt;"
        | '\n' -> "<br/>"
        | c -> String.make 1 c) str

let reducedEscape fmt str =
    String.iter (fun chr -> Format.fprintf fmt "%s" @@ (match chr with
        | '\n' -> "\\n"
        | '"' -> "\\\""
        | c -> String.make 1 c)) str

let draw_graph g =
    let contents = (Format.asprintf "drawDot (\"%a\")" reducedEscape g) in
    ignore @@ Js.Unsafe.eval_string contents

let text d v =
    let x = Html.createSpan d in
    x ## innerHTML <- Js.string v; x

let make_link caption action =
    let new_a = Html.createA Html.document in
    new_a ## href <- Js.string "";
    new_a ## onclick <- Html.handler (fun _ -> action (); Js._false);
    Dom.appendChild new_a (text Html.document caption);
    new_a

let checkbox where caption =
    let lbl = Html.createLabel Html.document in
    let inp = Html.createInput ~_type: (Js.string "checkbox") Html.document in
    Dom.appendChild lbl inp;
    Dom.appendChild lbl
        (text Html.document (" " ^ caption));
    Dom.appendChild where lbl;
    Dom.appendChild where (Html.createBr Html.document);
    inp


let make_example textbox (caption, code) =
    Dom.appendChild
        (Dom_html.getElementById "examples")
        (make_link caption (fun () -> textbox##value <- Js.string code));
    Dom.appendChild
        (Dom_html.getElementById "examples")
        (text Dom_html.document " - ")

let init _ =
    let d = Html.document in
    let body = Dom_html.getElementById "main" in
    let textbox = Html.createTextarea d in
    let inputbox_outcomes = Html.createInput ~_type: (Js.string "submit") d in
    let errorbox = Dom_html.getElementById "resulterror" in

    let do_outcomes _ =
        (try
            let code = Js.to_string (textbox ## value) in
            let dot =
                Format.asprintf "%a" Printer.dotOfStrategy
                    LinearLambda.(LlamInterpret.stratOfTerm
                            @@ LlamHelpers.lambdaize @@ code) in
            errorbox##innerHTML <- Js.string "" ;
            draw_graph dot
        with e ->
            draw_graph "digraph {}";
            errorbox##innerHTML <- Js.string @@
                Format.asprintf "<span class=\"errorhead\">Error:</span> %a"
                    escapeHtml (Printexc.to_string e));
        Js._false
    in

    inputbox_outcomes##value <- Js.string "Draw";
    textbox##className <- Js.string "codefont" ;
    textbox##rows <- 10; textbox##cols <- 50;
    textbox##value <- Js.string "((\\x:P . x) 1) || 1";
    inputbox_outcomes##onclick <- Dom_html.handler do_outcomes;
    Dom.appendChild body textbox;
    Dom.appendChild body (text d "<br />");
    Dom.appendChild body inputbox_outcomes;
    List.iter (make_example textbox)
    (* TODO: modify the examples *)
        ["Sequential program.",
         "x := 1; r <- x; x := r + 1";
         "Simple race.",
         "x := 1 || x := 2 || r <- x";
         "Commuting read/writes.",
         "r <- y; x := 1 || r' <- x; y := 1 ";
         "Store buffering",
         "x := 1; r <- x; r' <- y
|| y := 1; s <- y; s' <- x"
        ];

    Js._false

let () =
    Dom_html.window##onload <- Dom_html.handler init
