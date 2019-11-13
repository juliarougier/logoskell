import Prelude

turn::(Floating a) => (a,a) -> a
turn (old,val)=new
    where new = old + val
