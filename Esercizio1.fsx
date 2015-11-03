(*
    Danilo Cianfrone, mat. 501292
    Esercizio 1

        Avendo delle funzioni di interpolazione (es. easeInQuad, prese da http://gizma.com/easing) possiamo calcolare
        i valori intermedi dato una posizione finale desiderata per l'oggetto da animare.

        Ogni oggetto, rappresentato qui come GameObject(), avrà la sua matrice di trasformazione affine.
        Quindi, nell'evento Paint, basterà prendere come riferimento la matrice di trasformazione affine dell'oggetto
        da disegnare e passare i boundings dell'oggetto spesso (come visto a lezione), per poi ripristinare il contesto
        grafico.
        
        Nell'oggetto GameObject() ho incluso anche il metodo di interpolazione che lavora sulla matrice di trasformazione:
        il metodo calcola il nuovo valore (della componente X in questo caso) grazie alla funzione di interpolazione, ed
        applica una traslazione alla matrice di una quantità

                        newValue - offsetValue      (newValue: valore appena calcolato dalla funzione di interpolazione)
                                                    (offsetValue: coefficiente di traslazione della matrice di trasformazione)

        In questo script viene utilizzato un timer, che inizia con la pressione del tasto A, che permette di iniziare
        l'animazione, salvando il tempo di inizio alla pressione del tasto, e ad ogni Tick calcola il tempo corrente;
        la differenza in millisecondi viene passata all'oggetto da animare, mentre la durata è impostata a 600ms.

*)

open System.Windows.Forms
open System.Drawing

(* Interpolazione quadratica
    Math.easeInQuad = function (t, b, c, d) {
	    t /= d;
	    return c*t*t + b;
    }:
*)
let easeInQuad (total: float32) (start: float32) (dx: float32) (dur: float32) =
    let newT = total / dur
    dx * newT * newT + start

(* Interpolazione lineare
    Math.linearTween = function (t, b, c, d) {
	    return c*t/d + b;
    };
*)
let linearTween (total: float32) (start: float32) (dx: float32) (dur: float32) =
    dx * (total / dur) + start

(* Circular easing in
    Math.easeInCirc = function (t, b, c, d) {
	    t /= d;
	    return -c * (Math.sqrt(1 - t*t) - 1) + b;
    };
*)
let easeInCirc (total: float32) (start: float32) (dx: float32) (dur: float32) =
    let newT = total / dur
    (-dx) * (sqrt(1.f - newT * newT) - 1.f) + start

// ------------------------------------------------------------------------------------------------------ //
type GameObject() =
    let pos = PointF(0.f, 0.f)
    let siz = SizeF(20.f, 20.f)
    
    // Matrice per trasformazioni
    let startMtx         = new Drawing2D.Matrix()
    let mutable movinMtx = new Drawing2D.Matrix()

    member this.ModelSpace = movinMtx
    member this.Bounds = RectangleF(pos, siz)

    member this.Interpolation (total: float32) (where: Drawing2D.Matrix) (dur: float32) 
        (interFoo : float32 -> float32 -> float32 -> float32 -> float32) =
        // Interpolazione lungo asse X
        if (total >= dur) then ()
        else
            // Calcola il nuovo valore con la funzione d'interpolazione specificata
            let strElems = startMtx.Elements
            let mtxElems = movinMtx.Elements
            let finElems = where.Elements
            let newElems = new ResizeArray<float32>()

            for i in { 0 .. (strElems.Length - 1) } do
                newElems.Add(interFoo total strElems.[i] (finElems.[i] - mtxElems.[i]) dur)

            movinMtx.Dispose()
            movinMtx <- new Drawing2D.Matrix(newElems.[0], newElems.[1], newElems.[2], newElems.[3], newElems.[4], newElems.[5])

// ------------------------------------------------------------------------------------------------------ //

// ------------------------------------------------------------------------------------------------------ //
type myControl() as this =
    inherit UserControl()

    let obj = new GameObject()
    let mutable startAnim = System.DateTime.Now
    // Posizione finale da applicare all'oggetto
    let finalMtx = new Drawing2D.Matrix(3.f, 0.f, 0.f, 3.f, 300.f, 300.f)

    let timer = new Timer()
    do
        timer.Interval <- 17
        timer.Tick.Add(fun _ ->
            let nowTime = System.DateTime.Now
            let totalT  = (nowTime - startAnim).TotalMilliseconds

            obj.Interpolation (float32(totalT)) finalMtx 3000.f easeInQuad
            this.Invalidate()
        )

    // Implementazione double buffering
    let mutable (buffer: Bitmap) = null
    let updateBuffer() =
        if buffer = null || buffer.Width <> this.Width || buffer.Height <> this.Height then
            if buffer <> null then buffer.Dispose()
            buffer <- new Bitmap(this.Width, this.Height)

    override this.OnKeyDown e =
        match e.KeyCode with
        | Keys.A -> timer.Start(); startAnim <- System.DateTime.Now
        | _ -> ()

    override this.OnPaintBackground e =
        ()

    override this.OnPaint e =
        updateBuffer()
        let g = e.Graphics
        let gb = Graphics.FromImage(buffer)

        // Disegna lo sfondo
        gb.FillRectangle(Brushes.White, 0, 0, buffer.Width, buffer.Height)

        // Salva il contesto grafico del buffer
        let saveC = gb.Save()
        // Cambia le coordinate
        gb.Transform <- obj.ModelSpace

        // Disegna sul buffer
        gb.FillEllipse(Brushes.Red, obj.Bounds)
        gb.DrawEllipse(Pens.Black, obj.Bounds)
        // Ripristina il contesto grafico del buffer
        gb.Restore(saveC)

        // Scrivi sulla form
        g.DrawImage(buffer, 0, 0)

// ------------------------------------------------------------------------------------------------------ //

let form = new Form(Text = "Esercizio 1 - IUM Midterm 2015", TopMost = true, ClientSize = Size(320, 240))
let control = new myControl(Dock = DockStyle.Fill)
form.MinimumSize <- Size(600, 400)
form.Controls.Add(control)
form.Show()