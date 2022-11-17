Js.log("Hello, World!")
module BigStep = {
    type rec expr = | Cst(int) | Add(expr, expr) | Mul(expr, expr)

    let rec eval = (expr) => {
        switch expr {
        | Cst(i) => i
        | Add(a, b) => eval(a) + eval(b)
        | Mul(a, b) => eval(a) * eval(b)
        }
    }
}

module SmallStep = {
    type instr = Cst(int) | Add | Mul
    type instrs = list <instr>
    type operand = int
    type stack = list <operand>

    let rec eval = (instrs: instrs, stk: stack) => {
        switch (instrs, stk) {
        | (list{Cst(i), ... rest}, _) => 
            eval(rest, list{i, ...stk})
        | (list{Add, ... rest}, list{a, b, ... stk}) => 
            eval(rest, list{a + b, ... stk})
        | (list{Mul, ... rest}, list{a, b, ... stk}) => 
            eval(rest, list{a * b, ... stk})
        | (list{}, list{res}) => res
        | _ => assert false
        }
    }    
}

// Belt.List.concatMang可以把嵌套的list展开(拍平)
let rec compile = (source: BigStep.expr): SmallStep.instrs => {
    switch source {
    | Cst(i) => list{Cst(i)}
    | Add(a, b) => Belt.List.concatMany([compile(a), compile(b), list{Add}])
    | Mul(a, b) => Belt.List.concatMany([compile(a), compile(b), list{Mul}])
    }
}

let exp: BigStep.expr = Add(Cst(5), Cst(2))
let instrs = compile(exp)
Js.log(instrs)
Js.log(SmallStep.eval(instrs, list{}))