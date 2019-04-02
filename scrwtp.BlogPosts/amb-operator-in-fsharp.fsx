type Cont<'a, 'r> = 
    private | Cont of (('a -> 'r) -> 'r)
    member this.RunCont (k: 'a -> 'r) = 
        match this with
        | Cont f -> f k
        
type ContinuationBuilder() =
    member this.Bind (ma : Cont<'a, 'r>, f: 'a -> Cont<'b, 'r>) : Cont<'b, 'r> = 
        Cont <| fun kb -> 
            ma.RunCont (fun a -> 
                let mb = f a
                mb.RunCont kb)
    member this.Return(x : 'a) : Cont<'a, 'r> = 
        Cont <| fun k -> k x
    member this.ReturnFrom(m : Cont<'a, 'r>) = m
    member this.Zero() = this.Return()

module Cont = 
    
    let run k (comp: Cont<'a, 'r>) = 
        comp.RunCont k

    let callCC (f: ('a -> Cont<'b, 'r>) -> Cont<'a, 'r>) : Cont<'a, 'r> = 
        Cont <| fun k ->
            let m = f (fun a ->
                Cont <| (fun _ -> k a)) 
            m.RunCont k

let cont = ContinuationBuilder()

module Amb = 

    type AmbContext<'a, 'r> = 
        private {
            mutable stack: (unit -> Cont<'a, 'r>) list 
        }
        static member Empty = { stack = List.empty<unit -> Cont<'a, 'r>> }
        member this.Fail () : Cont<'a, 'r> = 
            match this.stack with
            | [] -> failwith "Backtracking stack exhausted!"
            | x::xs -> 
                this.stack <- xs
                x ()
        member this.Push (k: unit -> Cont<'a, 'r>) = 
            this.stack <- k :: this.stack           
            
    let rec amb (ctx: AmbContext<'a,'r>) (choices: 'a list) : Cont<'a, 'r> = 
        cont {
            match choices with
            | [] -> 
                return! ctx.Fail()
            | x::xs ->
                return! Cont.callCC (fun exit ->
                    cont {
                        do! Cont.callCC (fun (k: unit -> Cont<'a, 'r>) -> 
                            cont {
                                ctx.Push(k)
                                do! exit x
                            })
                        return! amb ctx xs
                    })
        }

    let require (ctx: AmbContext<'a,'r>) (value: bool) = 
        cont {
            if not value then 
                let! _ = amb ctx []
                return ()
        }

(*

Baker, Cooper, Fletcher, Miller, and Smith live on different floors of an apartment house that contains only five
floors. 

Baker does not live on the top floor. Cooper does not live on the bottom floor. Fletcher does not live on either the top or the boom floor. 
Miller lives on a higher floor than does Cooper. Smith does not live on a floor adjacent to Fletcher’s. Fletcher does not live on a floor adjacent to Cooper’s. 

Where does everyone live?

*)

let puzzle = 
    cont {
        let ctx = Amb.AmbContext.Empty

        // Baker, Cooper, Fletcher, Miller, and Smith live on different floors of an apartment house that contains only five floors. 
        let! baker      = Amb.amb ctx [ 1 .. 5 ]
        let! cooper     = Amb.amb ctx [ 1 .. 5 ]
        let! fletcher   = Amb.amb ctx [ 1 .. 5 ]
        let! miller     = Amb.amb ctx [ 1 .. 5 ]
        let! smith      = Amb.amb ctx [ 1 .. 5 ]
        
        let all = [ baker; cooper; fletcher; miller; smith ]

        do! Amb.require ctx (List.distinct all = all)

        // Baker does not live on the top floor. 
        do! Amb.require ctx (baker <> 5)
        
        // Cooper does not live on the bottom floor. 
        do! Amb.require ctx (cooper <> 1)
        
        // Fletcher does not live on either the top or the bottom floor. 
        do! Amb.require ctx (fletcher <> 1 && fletcher <> 5)

        // Miller lives on a higher floor than does Cooper. 
        do! Amb.require ctx (miller > cooper)

        // Smith does not live on a floor adjacent to Fletcher’s. 
        do! Amb.require ctx (abs (smith - fletcher) > 1)

        // Fletcher does not live on a floor adjacent to Cooper’s. 
        do! Amb.require ctx (abs (fletcher - cooper) > 1)

        return 
            [
                "baker"     , baker   
                "cooper"    , cooper  
                "fletcher"  , fletcher
                "miller"    , miller  
                "smith"     , smith   
            ]
    }

puzzle |> Cont.run id