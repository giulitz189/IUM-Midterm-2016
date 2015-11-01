#load "libLWCs.fsx"

(*
    Danilo Cianfrone - mat. 501292
    Esercizio 2 - Curve Editor

    La realizzazione del Curve Editor potenziato, grazie alle librerie modificate, permette
    di implementare comandi in coordinate mondo, che verranno costantemente aggiornate in
    coordinate vista in caso di trasformazioni affini.

    Un esempio di tale comando è il bottone RES, presente al fianco della curva di Bezièr.
    Tale bottone permette di resettare le matrici V2W e W2V annullando tutte le trasformazioni
    avvenute fino al momento della pressione.

    Inoltre, sempre grazie alla libreria estesa, abbiamo modo di vedere le prodezze del double
    buffering, (quasi) completamente senza flickering.

    I tasti più piccoli, T+ e T-, servono a diminuire ed aumentare la tensione della
    curva blu, fino ad un minimo di 1 e un massimo non meglio definito.

*)

open System.Windows.Forms
open System.Drawing
open libLWCs

// ---------------------------------------------------------------------------------------------------------------- //

type IUMButton(loc: PointF, siz: SizeF, mode: LWCmode, par: Control) =
    inherit LWC(loc, siz, mode, par)

    let clickEvt = new Event<System.EventArgs>()
    let upEvt    = new Event<MouseEventArgs>()
    let downEvt  = new Event<MouseEventArgs>()
    let moveEvt  = new Event<MouseEventArgs>()

    let mutable text = ""

    member this.Click     = clickEvt.Publish
    member this.MouseDown = downEvt.Publish
    member this.MouseMove = moveEvt.Publish
    member this.MouseUp   = upEvt.Publish

    member this.Text with get() = text and set(v) = text <- v

    override this.OnMouseUp e =
        upEvt.Trigger(e)
        clickEvt.Trigger(new System.EventArgs())

    override this.OnMouseMove e =
        moveEvt.Trigger(e)

    override this.OnMouseDown e =
        downEvt.Trigger(e)

    override this.OnPaint e =
        let gCont = e.Graphics
        // Disegna il bottone
        gCont.FillEllipse(Brushes.Red, this.PViewLoc.X, this.PViewLoc.Y, this.Size.Width, this.Size.Height)
        // Disegna la stringa sul bottone
        let sz = gCont.MeasureString(text, this.Parent.Font)
        gCont.DrawString(text, this.Parent.Font, Brushes.White, 
            PointF(this.PViewLoc.X + sz.Width / 2.5f, this.PViewLoc.Y + sz.Height / 2.5f))
// ---------------------------------------------------------------------------------------------------------------- //

type NavBut =
    | Up    = 0
    | Right = 1
    | Left  = 2
    | Down  = 3

// ---------------------------------------------------------------------------------------------------------------- //

type CurveEditor() as this =
    inherit LWCcontainer()

    // Punti della curva di Beziér in cordinate mondo
    let bezierPts = [| PointF(); PointF(20.f, 20.f); PointF(50.f, 50.f); PointF(50.f, 100.f) |]

    let buttons = [| new IUMButton(PointF(32.f, 0.f),  SizeF(32.f, 32.f), LWCmode.ViewMode, this, Text = "U");
                     new IUMButton(PointF(64.f, 32.f), SizeF(32.f, 32.f), LWCmode.ViewMode, this, Text = "R");
                     new IUMButton(PointF(0.f, 32.f),  SizeF(32.f, 32.f), LWCmode.ViewMode, this, Text = "L");
                     new IUMButton(PointF(32.f, 64.f), SizeF(32.f, 32.f), LWCmode.ViewMode, this, Text = "D") |]

    let worldButton = new IUMButton(PointF(200.f, 200.f),  SizeF(32.f, 32.f), LWCmode.WorldMode, this, Text = "RES")
    let tensButton  = [| new IUMButton(PointF(0.f, 100.f),  SizeF(20.f, 20.f), LWCmode.ViewMode, this, Text = "T+");
                         new IUMButton(PointF(0.f, 125.f),  SizeF(20.f, 20.f), LWCmode.ViewMode, this, Text = "T-") |]

    // Aggiungi i bottoni al container di LWCs
    do 
        buttons    |> Seq.iter (fun b -> this.LWControls.Add(b))
        tensButton |> Seq.iter (fun b -> this.LWControls.Add(b))
        this.LWControls.Add(worldButton)

    // Accesso ai bottoni per codice enumerato
    let button (k: NavBut) =
        buttons.[int(k)]

    // Indica il vettore per lo scrolling (traslazione)
    let scrollBy dir =
        match dir with
        | NavBut.Up    -> (0.f, -10.f)
        | NavBut.Down  -> (0.f,  10.f)
        | NavBut.Left  -> (-10.f, 0.f)
        | NavBut.Right -> (10.f,  0.f)

    // Funzioni di traslazione -------------------------------------- //
    let TranslateW (tX, tY) =
        this.W2V.Translate(tX, tY)
        this.V2W.Translate(-tX, -tY, Drawing2D.MatrixOrder.Append)

    let Translate (x, y) =
        let t = [| PointF(0.f, 0.f); PointF(x, y) |]
        this.V2W.TransformPoints(t)
        TranslateW (t.[1].X - t.[0].X, t.[1].Y - t.[0].Y)
    // -------------------------------------------------------------- //

    // Funzione di rotazione attorno ad un punto
    let RotateAtW point angle =
        this.W2V.RotateAt(angle, point)
        this.V2W.RotateAt(-angle, point, Drawing2D.MatrixOrder.Append)

    // Funzione di scalatura
    let ScaleW (sX, sY) =
        this.W2V.Scale(sX, sY)
        this.V2W.Scale(1.f/sX, 1.f/sY, Drawing2D.MatrixOrder.Append)

    // Trasformazione punti in coordinate mondo
    let TransformPW (p: Point) =
        let point = [| PointF(single p.X, single p.Y) |]
        this.V2W.TransformPoints(point)
        point.[0]

    // Handle per i tasti premuti
    let handleKeys (k: Keys) =
        match k with
        // Traslazione
        | Keys.W -> scrollBy NavBut.Up    |> Translate; this.Invalidate()
        | Keys.A -> scrollBy NavBut.Left  |> Translate; this.Invalidate()
        | Keys.S -> scrollBy NavBut.Down  |> Translate; this.Invalidate()
        | Keys.D -> scrollBy NavBut.Right |> Translate; this.Invalidate()
        // Rotazione
        | Keys.Q -> let p = TransformPW (Point(this.Width / 2, this.Height / 2))
                    RotateAtW p 10.f
                    this.Invalidate()
        | Keys.E -> let p = TransformPW (Point(this.Width / 2, this.Height / 2))
                    RotateAtW p -10.f
                    this.Invalidate()
        // Scalatura
        | Keys.Z -> let pScale = TransformPW (Point(this.Width / 2, this.Height / 2))
                    ScaleW(1.1f, 1.1f)
                    this.Invalidate()
        | Keys.X -> let pScale = TransformPW (Point(this.Width / 2, this.Height / 2))
                    ScaleW(1.f/1.1f, 1.f/1.1f)
                    this.Invalidate()
        | _ -> ()

    // Scroll-timer
    let scrollTimer         = new Timer()
    let mutable scrollDir   = NavBut.Up
    do
        // Registrazione codice tick
        scrollTimer.Interval <- 30
        scrollTimer.Tick.Add(fun _ ->
            scrollBy scrollDir |> Translate
            this.Invalidate()
        )

        // Ad ogni MouseDown sui bottoni LW, registrare il nuovo scrollDir
        buttons.[int(NavBut.Up)   ].MouseDown.Add(fun _ -> scrollDir <- NavBut.Up)
        buttons.[int(NavBut.Down) ].MouseDown.Add(fun _ -> scrollDir <- NavBut.Down)
        buttons.[int(NavBut.Left) ].MouseDown.Add(fun _ -> scrollDir <- NavBut.Left)
        buttons.[int(NavBut.Right)].MouseDown.Add(fun _ -> scrollDir <- NavBut.Right)
        // Riallinea la posizione del mondo e della vista
        worldButton.MouseDown.Add(fun _ -> 
            this.V2W <- new Drawing2D.Matrix()
            this.W2V <- new Drawing2D.Matrix()
            this.Invalidate()
        )
        // Aumenta tensione della curva
        tensButton.[0].MouseDown.Add(fun _ -> this.Tension <- this.Tension + 1.f; this.Invalidate())
        tensButton.[1].MouseDown.Add(fun _ -> 
            if this.Tension > 1.f then this.Tension <- this.Tension - 1.f; this.Invalidate() 
        )

        for v in [ NavBut.Up; NavBut.Left; NavBut.Right; NavBut.Down ] do
            let idx = int(v)
            buttons.[idx].MouseDown.Add(fun _ -> scrollTimer.Start())
            buttons.[idx].MouseUp.Add(fun _ -> scrollTimer.Stop())

    // Tensione della curva
    let mutable tension = 3.f
    // Raggio dell'handle
    let handleSize = 5.f

    // Punto selezionato
    let mutable pSelected  = None
    // Offset per il drag&drop
    let mutable offsetDrag = PointF()

    // Test per gli handle di modifica della curva
    let handleHitTest (point: PointF) (hit: PointF) =
        let x = point.X - hit.X
        let y = point.Y - hit.Y
        (x * x) + (y * y) < (handleSize * handleSize)

    member this.Tension with get() = tension and set(v) = tension <- v

    override this.OnKeyDown e =
        handleKeys e.KeyCode

    override this.OnMouseDown e =
        base.OnMouseDown(e)
        // Posizione del mouse in coordinate mondo
        let mPoint  = TransformPW e.Location
        let hitTest = handleHitTest mPoint
        // Trova i punti selezionati
        pSelected <- bezierPts |> Array.tryFindIndex hitTest
        match pSelected with
        | Some(idx) -> 
            let point = bezierPts.[idx]
            offsetDrag <- PointF(point.X - mPoint.X, point.Y - mPoint.Y)
        | None      -> ()

    override this.OnMouseUp e =
        base.OnMouseUp(e)
        // Nessun punto selezionato
        pSelected <- None

    override this.OnMouseMove e =
        let mPoint = TransformPW e.Location
        match pSelected with
        | Some(idx) -> 
            bezierPts.[idx] <- PointF(mPoint.X + offsetDrag.X, mPoint.Y + offsetDrag.Y)
            this.Invalidate()
        | None      -> ()

    override this.OnPaint e =
        let gCont     = this.UseBuffer()
        let gContSave = gCont.Save()

        // La curva è in cordinate mondo, trasforma in cordinate vista
        gCont.Transform <- this.W2V
        gCont.DrawBezier(Pens.Black, bezierPts.[0], bezierPts.[1], bezierPts.[2], bezierPts.[3])
        gCont.DrawLine(Pens.Red, bezierPts.[0], bezierPts.[1])
        gCont.DrawLine(Pens.Red, bezierPts.[2], bezierPts.[3])
        gCont.DrawCurve(Pens.Blue, bezierPts, tension)

        // Disegna i cerchi degli handle
        bezierPts |> Seq.iter (fun p ->
            let w = 5.f
            gCont.DrawEllipse(Pens.Green, p.X - w, p.Y - w, 2.f * w, 2.f * w)
        )

        // Ripristina il contesto grafico
        gCont.Restore(gContSave)

        base.OnPaint(e)

// ---------------------------------------------------------------------------------------------------------------- //

let myForm = new Form(Text = "Esercizio 2 - IUM Midterm - Curve Editor", TopMost = true)
let myEdit = new CurveEditor(Dock = DockStyle.Fill)
// Dimensione minima della form
myForm.MinimumSize <- Size(600, 400)
myForm.Controls.Add(myEdit)
myEdit.Focus()
myForm.Show()