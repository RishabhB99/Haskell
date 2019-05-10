import Data.Function (on)
import Data.List (sortBy)
fun str = [ (x,c) | x<-['A'..'z'], let c = (length.filter (==x)) str,c>0]
small str=[ (x,c) | x<-['a'..'z'], let c = (length.filter (==x)) str]
large str=[ (x,c) | x<-['A'..'Z'], let c = (length.filter (==x)) str,c>0]
s list = sum([snd(x,c)|(x,c)<-list])

--compare `on` snd :: (a1, a) -> (a1, a) -> Ordering
sort l = sortBy (compare `on` snd) l
sorted l= sortBy (compare `on` fst) l
main = do
    str <- getLine
    putStrLn(str)
    let f=fun(str)
        --l = [(x,(i`div`su)*100) |(x,i)<-small(str)]
        sm=sort(small(str))
        temp2=large(str)
        su=s(f)
        eng=sort([('a',8.55),('b',1.60),('c',3.16),('d',3.87),('e',12.1),('f',2.18),('g',2.09),('h',4.96),('i',7.33),('j',0.22),('k',0.81),('l',4.21),('m',2.53),('n',7.17),('o',7.47),('p',2.07),('q',0.1),('r',6.33),('s',6.73),('t',8.94),('u',2.68),('v',1.06),('w',1.83),('x',0.19),('y',1.72),('z',0.11)])
    print(sm)
    print(eng)
    let key=[(fst(sm!!i),fst(eng!!i))|i<-[0..25]]
        l=sorted(key)
    print(key)
    print(l)
    let repl 'a' =snd(l!!0)
        repl 'b' =snd(l!!1)
        repl 'c' =snd(l!!2)
        repl 'd' =snd(l!!3)
        repl 'e' =snd(l!!4)
        repl 'f' =snd(l!!5)
        repl 'g' =snd(l!!6)
        repl 'h' =snd(l!!7)
        repl 'i' =snd(l!!8)
        repl 'j' =snd(l!!9)
        repl 'k' =snd(l!!10)
        repl 'l' =snd(l!!11)
        repl 'm' =snd(l!!12)
        repl 'n' =snd(l!!13)
        repl 'o' =snd(l!!14)
        repl 'p' =snd(l!!15)
        repl 'q' =snd(l!!16)
        repl 'r' =snd(l!!17)
        repl 's' =snd(l!!18)
        repl 't' =snd(l!!19)
        repl 'u' =snd(l!!20)
        repl 'v' =snd(l!!21)
        repl 'w' =snd(l!!22)
        repl 'x' =snd(l!!23)
        repl 'y' =snd(l!!24)
        repl 'z' =snd(l!!25)
        in map repl str
    print(str)
    --let ans = [snd(l!!(ord(i)-ord('a')))|i<-str,i<-['a'..'z']]
    --print(ans)
