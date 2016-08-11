module Html = Dom_html


let draw_graph g =
    ignore @@ Js.Unsafe.eval_string (Printf.sprintf "drawDot (%S)" g)

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

    let do_outcomes _ =
        (try
            let code = Js.to_string (textbox ## value) in
            let buff = Buffer.create 512 in
            let fmt = Format.formatter_of_buffer buff in
            Printer.dotOfStrategy fmt
                    LinearLambda.(LlamInterpret.stratOfTerm
                            @@ LlamHelpers.lambdaize @@ code) ;
            let dot = Buffer.contents buff in
            draw_graph dot;
        with e ->
            ignore @@ Js.Unsafe.eval_string
                (Format.sprintf "alert(\"%s\");" @@ Printexc.to_string e));
        Js._false
    in

    inputbox_outcomes##value <- Js.string "Draw";
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
