datatype Proposicion =
 constante of bool
| variable of string
| negacion of Proposicion
| conjuncion of Proposicion * Proposicion
| disyuncion of Proposicion * Proposicion
| implicacion of Proposicion * Proposicion
| equivalencia of Proposicion * Proposicion
;