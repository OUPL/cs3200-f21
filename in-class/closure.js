"use strict"

function f() {
    let x = 123;
    return function() {
	console.log(x)
    }
}

function h(g) {
    let x = 5;
    g()
}

 let g = f();
// g()

 h(g)
