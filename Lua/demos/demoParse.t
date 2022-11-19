  $ ./demoParse.exe <<-EOF
  > 34 'string =)' {1, 2}
  > EOF
  [(LuaExpr (LuaConst (LuaNumber 34.)));
    (LuaExpr (LuaConst (LuaString "string =)")));
    (LuaExpr
       (LuaTableInit
          [(JustExpr (LuaConst (LuaNumber 1.)));
            (JustExpr (LuaConst (LuaNumber 2.)))]))
    ]
  $ ./demoParse.exe <<-EOF
  > a[b], c = 3, 4, 5
  > EOF
  [(LuaSet ([(Index ((Ident "a"), (LuaVariable "b"))); (Ident "c")],
      [(LuaConst (LuaNumber 3.)); (LuaConst (LuaNumber 4.));
        (LuaConst (LuaNumber 5.))]
      ))
    ]
  $ ./demoParse.exe <<-EOF
  > {} {b=3} {1} {1, 2} {1, b=2} {{}} {{}, 123} {[x] = x}
  > EOF
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
  $ ./demoParse.exe <<-EOF
  > a.b = 3 a.b.b.b = 3 a[b].c[d] = 3 a[4 + 1] = 3 + 2
  > EOF
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
           (LuaBinOp ((AOp Add), (LuaConst (LuaNumber 4.)),
              (LuaConst (LuaNumber 1.))))
           ))
         ],
       [(LuaBinOp ((AOp Add), (LuaConst (LuaNumber 3.)),
           (LuaConst (LuaNumber 2.))))
         ]
       ))
    ]
  $ ./demoParse.exe <<-EOF
  > if a then b elseif c then d else e end
  > if a then b end
  > if a then b else c end
  > for i=1,2,3 do end
  > for i=1,2 do end
  > while a do b end
  > repeat a until b
  > return
  > return 4
  > break
  > EOF
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
  $ ./demoParse.exe <<-EOF
  > 1 "3" true b {}
  > a(1, {}, b)
  > a.b a[b] 
  > 1 + 2
  > 1 + 2 * 3
  > 1 * 2 + 3
  > 1 + 2 * 3 + 4 * 5 ^ 6
  > 1 + 2 * 3 - 4 - 5 - 6
  > 1 ^ 2 - 3 * 5 ^ 5 - 7
  > -5
  > -5 + 1
  > not false
  > not true or 4
  > not true and 4 or false
  > not true and not false or true
  > true and not false
  > EOF
  [(LuaExpr (LuaConst (LuaNumber 1.))); (LuaExpr (LuaConst (LuaString "3")));
    (LuaExpr (LuaConst (LuaBool true))); (LuaExpr (LuaVariable "b"));
    (LuaExpr (LuaTableInit []));
    (LuaStatementApply
       (LuaCall ((LuaVariable "a"),
          [(LuaConst (LuaNumber 1.)); (LuaTableInit []); (LuaVariable "b")])));
    (LuaExpr (LuaTableGet ((LuaVariable "a"), (LuaConst (LuaString "b")))));
    (LuaExpr (LuaTableGet ((LuaVariable "a"), (LuaVariable "b"))));
    (LuaExpr
       (LuaBinOp ((AOp Add), (LuaConst (LuaNumber 1.)),
          (LuaConst (LuaNumber 2.)))));
    (LuaExpr
       (LuaBinOp ((AOp Add), (LuaConst (LuaNumber 1.)),
          (LuaBinOp ((AOp Mul), (LuaConst (LuaNumber 2.)),
             (LuaConst (LuaNumber 3.))))
          )));
    (LuaExpr
       (LuaBinOp ((AOp Add),
          (LuaBinOp ((AOp Mul), (LuaConst (LuaNumber 1.)),
             (LuaConst (LuaNumber 2.)))),
          (LuaConst (LuaNumber 3.)))));
    (LuaExpr
       (LuaBinOp ((AOp Add),
          (LuaBinOp ((AOp Add), (LuaConst (LuaNumber 1.)),
             (LuaBinOp ((AOp Mul), (LuaConst (LuaNumber 2.)),
                (LuaConst (LuaNumber 3.))))
             )),
          (LuaBinOp ((AOp Mul), (LuaConst (LuaNumber 4.)),
             (LuaBinOp ((AOp Pow), (LuaConst (LuaNumber 5.)),
                (LuaConst (LuaNumber 6.))))
             ))
          )));
    (LuaExpr
       (LuaBinOp ((AOp Sub),
          (LuaBinOp ((AOp Sub),
             (LuaBinOp ((AOp Sub),
                (LuaBinOp ((AOp Add), (LuaConst (LuaNumber 1.)),
                   (LuaBinOp ((AOp Mul), (LuaConst (LuaNumber 2.)),
                      (LuaConst (LuaNumber 3.))))
                   )),
                (LuaConst (LuaNumber 4.)))),
             (LuaConst (LuaNumber 5.)))),
          (LuaConst (LuaNumber 6.)))));
    (LuaExpr
       (LuaBinOp ((AOp Add),
          (LuaBinOp ((AOp Sub),
             (LuaBinOp ((AOp Sub),
                (LuaBinOp ((AOp Sub),
                   (LuaBinOp ((AOp Sub),
                      (LuaBinOp ((AOp Pow), (LuaConst (LuaNumber 1.)),
                         (LuaConst (LuaNumber 2.)))),
                      (LuaBinOp ((AOp Mul), (LuaConst (LuaNumber 3.)),
                         (LuaBinOp ((AOp Pow), (LuaConst (LuaNumber 5.)),
                            (LuaConst (LuaNumber 5.))))
                         ))
                      )),
                   (LuaConst (LuaNumber 7.)))),
                (LuaConst (LuaNumber 5.)))),
             (LuaConst (LuaNumber 5.)))),
          (LuaConst (LuaNumber 1.)))));
    (LuaExpr (LuaUnOp (Not, (LuaConst (LuaBool false)))));
    (LuaExpr
       (LuaBinOp ((LOp Or), (LuaUnOp (Not, (LuaConst (LuaBool true)))),
          (LuaConst (LuaNumber 4.)))));
    (LuaExpr
       (LuaBinOp ((LOp Or),
          (LuaBinOp ((LOp And), (LuaUnOp (Not, (LuaConst (LuaBool true)))),
             (LuaConst (LuaNumber 4.)))),
          (LuaConst (LuaBool false)))));
    (LuaExpr
       (LuaBinOp ((LOp Or),
          (LuaBinOp ((LOp And), (LuaUnOp (Not, (LuaConst (LuaBool true)))),
             (LuaUnOp (Not, (LuaConst (LuaBool false)))))),
          (LuaConst (LuaBool true)))));
    (LuaExpr
       (LuaBinOp ((LOp And), (LuaConst (LuaBool true)),
          (LuaUnOp (Not, (LuaConst (LuaBool false)))))))
    ]
