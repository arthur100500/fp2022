  $ ./demoParse.exe
  [(LuaExpr (LuaConst (LuaNumber 34.)));
    (LuaExpr (LuaConst (LuaString "string =)")));
    (LuaExpr
       (LuaTableInit
          [(JustExpr (LuaConst (LuaNumber 1.)));
            (JustExpr (LuaConst (LuaNumber 2.)))]))
    ]
  [(LuaSet ([(Index ((Ident "a"), (LuaVariable "b"))); (Ident "c")],
      [(LuaConst (LuaNumber 3.)); (LuaConst (LuaNumber 4.));
        (LuaConst (LuaNumber 5.))]
      ))
    ]
  [(LuaExpr (LuaTableInit []));
    (LuaExpr
       (LuaTableInit
          [(PairExpr ((LuaConst (LuaString "b")), (LuaConst (LuaNumber 3.))))]));
    (LuaExpr (LuaTableInit [(JustExpr (LuaConst (LuaNumber 1.)))]));
    (LuaExpr
       (LuaTableInit
          [(JustExpr (LuaConst (LuaNumber 1.)));
            (JustExpr (LuaConst (LuaNumber 2.)))]));
    (LuaExpr
       (LuaTableInit
          [(JustExpr (LuaConst (LuaNumber 1.)));
            (PairExpr ((LuaConst (LuaString "b")), (LuaConst (LuaNumber 2.))))]));
    (LuaExpr (LuaTableInit [(JustExpr (LuaTableInit []))]));
    (LuaExpr
       (LuaTableInit
          [(JustExpr (LuaTableInit [])); (JustExpr (LuaConst (LuaNumber 123.)))
            ]));
    (LuaExpr (LuaTableInit [(PairExpr ((LuaVariable "x"), (LuaVariable "x")))]))
    ]
  [(LuaSet ([(Index ((Ident "a"), (LuaConst (LuaString "b"))))],
      [(LuaConst (LuaNumber 3.))]));
    (LuaSet (
       [(Index (
           (Index ((Index ((Ident "a"), (LuaConst (LuaString "b")))),
              (LuaConst (LuaString "b")))),
           (LuaConst (LuaString "b"))))
         ],
       [(LuaConst (LuaNumber 3.))]));
    (LuaSet (
       [(Index (
           (Index ((Index ((Ident "a"), (LuaVariable "b"))),
              (LuaConst (LuaString "c")))),
           (LuaVariable "d")))
         ],
       [(LuaConst (LuaNumber 3.))]));
    (LuaSet (
       [(Index ((Ident "a"),
           (LuaBinOp ("+", (LuaConst (LuaNumber 4.)), (LuaConst (LuaNumber 1.))
              ))
           ))
         ],
       [(LuaBinOp ("+", (LuaConst (LuaNumber 3.)), (LuaConst (LuaNumber 2.))))]
       ))
    ]
  [(LuaIf ((LuaVariable "a"), [(LuaExpr (LuaVariable "b"))],
      [((LuaVariable "c"), [(LuaExpr (LuaVariable "d"))])],
      (Some [(LuaExpr (LuaVariable "e"))])));
    (LuaIf ((LuaVariable "a"), [(LuaExpr (LuaVariable "b"))], [], None));
    (LuaIf ((LuaVariable "a"), [(LuaExpr (LuaVariable "b"))], [],
       (Some [(LuaExpr (LuaVariable "c"))])));
    (LuaFornum ("i", (LuaConst (LuaNumber 1.)), (LuaConst (LuaNumber 2.)),
       (Some (LuaConst (LuaNumber 3.))), []));
    (LuaFornum ("i", (LuaConst (LuaNumber 1.)), (LuaConst (LuaNumber 2.)),
       None, []));
    (LuaWhile ((LuaVariable "a"), [(LuaExpr (LuaVariable "b"))]));
    (LuaRepeat ([(LuaExpr (LuaVariable "a"))], (LuaVariable "b")));
    (LuaReturn None); (LuaReturn (Some (LuaConst (LuaNumber 4.)))); LuaBreak]
  [(LuaExpr (LuaConst (LuaNumber 1.))); (LuaExpr (LuaConst (LuaString "3")));
    (LuaExpr (LuaConst (LuaBool true))); (LuaExpr (LuaVariable "b"));
    (LuaExpr (LuaTableInit []));
    (LuaStatementApply
       (LuaCall ((LuaVariable "a"),
          [(LuaConst (LuaNumber 1.)); (LuaTableInit []); (LuaVariable "b")])));
    (LuaExpr (LuaTableGet ((LuaVariable "a"), (LuaConst (LuaString "b")))));
    (LuaExpr (LuaTableGet ((LuaVariable "a"), (LuaVariable "b"))));
    (LuaExpr
       (LuaBinOp ("+", (LuaConst (LuaNumber 1.)), (LuaConst (LuaNumber 2.)))));
    (LuaExpr
       (LuaBinOp ("+", (LuaConst (LuaNumber 1.)),
          (LuaBinOp ("*", (LuaConst (LuaNumber 2.)), (LuaConst (LuaNumber 3.))
             ))
          )));
    (LuaExpr
       (LuaBinOp ("+",
          (LuaBinOp ("*", (LuaConst (LuaNumber 1.)), (LuaConst (LuaNumber 2.))
             )),
          (LuaConst (LuaNumber 3.)))));
    (LuaExpr
       (LuaBinOp ("+",
          (LuaBinOp ("+", (LuaConst (LuaNumber 1.)),
             (LuaBinOp ("*", (LuaConst (LuaNumber 2.)),
                (LuaConst (LuaNumber 3.))))
             )),
          (LuaBinOp ("*", (LuaConst (LuaNumber 4.)),
             (LuaBinOp ("^", (LuaConst (LuaNumber 5.)),
                (LuaConst (LuaNumber 6.))))
             ))
          )));
    (LuaExpr
       (LuaBinOp ("-",
          (LuaBinOp ("-",
             (LuaBinOp ("-",
                (LuaBinOp ("+", (LuaConst (LuaNumber 1.)),
                   (LuaBinOp ("*", (LuaConst (LuaNumber 2.)),
                      (LuaConst (LuaNumber 3.))))
                   )),
                (LuaConst (LuaNumber 4.)))),
             (LuaConst (LuaNumber 5.)))),
          (LuaConst (LuaNumber 6.)))));
    (LuaExpr
       (LuaBinOp ("+",
          (LuaBinOp ("-",
             (LuaBinOp ("-",
                (LuaBinOp ("-",
                   (LuaBinOp ("-",
                      (LuaBinOp ("^", (LuaConst (LuaNumber 1.)),
                         (LuaConst (LuaNumber 2.)))),
                      (LuaBinOp ("*", (LuaConst (LuaNumber 3.)),
                         (LuaBinOp ("^", (LuaConst (LuaNumber 5.)),
                            (LuaConst (LuaNumber 5.))))
                         ))
                      )),
                   (LuaConst (LuaNumber 7.)))),
                (LuaConst (LuaNumber 5.)))),
             (LuaConst (LuaNumber 5.)))),
          (LuaConst (LuaNumber 1.)))));
    (LuaExpr (LuaUnOp ("not", (LuaConst (LuaBool false)))));
    (LuaExpr
       (LuaBinOp ("or", (LuaUnOp ("not", (LuaConst (LuaBool true)))),
          (LuaConst (LuaNumber 4.)))));
    (LuaExpr
       (LuaBinOp ("or",
          (LuaBinOp ("and", (LuaUnOp ("not", (LuaConst (LuaBool true)))),
             (LuaConst (LuaNumber 4.)))),
          (LuaConst (LuaBool false)))));
    (LuaExpr
       (LuaBinOp ("or",
          (LuaBinOp ("and", (LuaUnOp ("not", (LuaConst (LuaBool true)))),
             (LuaUnOp ("not", (LuaConst (LuaBool false)))))),
          (LuaConst (LuaBool true)))));
    (LuaExpr
       (LuaBinOp ("and", (LuaConst (LuaBool true)),
          (LuaUnOp ("not", (LuaConst (LuaBool false)))))))
    ]
