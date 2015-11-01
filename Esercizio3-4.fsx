#load ".\Esercizio2\libLWCs.fsx"

(*
    Danilo Cianfrone - mat. 501292
    Esercizio 3-4

    Nel mio fantastico editor, abbiamo una dock laterale con bottoni LWC in grado di manipolare
    il foglio di disegno vettoriale al suo fianco.

    I quattro tasti in alto sono navigatori del foglio di disegno;
    i quattro tasti in basso permettono di aggiungere una primitiva grafica al disegno:
    ellisse, rettangolo, linea e curva di Bezièr;
    l'ultimo tasto abilita le modifiche alle primitive grafiche.

    L'editor utilizza uno stato interno per discernere tra i comportamenti da adottare nel
    caso di un evento (es. MouseDown, ...).
    In particolare, avremo 6 stati differenti:

        type StateEditor =
            | FreeTransform = 0 -> abilita modifiche alle primitive
            | InsertCircle  = 1 -> inserisce un ellisse ("circle" è frutto di sbadataggine)
            | InsertRectang = 2 -> inserisce un rettangolo
            | InsertLine    = 3 -> inserisce una linea
            | InsertBezier  = 4 -> inserisce una curva di Bezièr
            | MoveThings    = 5 -> stato default: drag&drop ed eliminazione di primitive

        N.B. il drag&drop e l'eliminazione non sono supportate per le curve di Bezièr
             (ho avuto problemi con lo hitTest).

    Nello stato MoveThings, l'editor permette il drag&drop col tasto sinistro del mouse,
    e l'eliminazione di una primitiva col tasto destro.

    Nello stato FreeTransform, l'editor permette di modificare i punti di disegno delle
    primitive col tasto sinistro sull'handler; il tasto destro riporta l'editor
    nello stato di default.

    I comandi da tastiera sono gli stessi utilizzati nel Curve Editor dell'Esercizio 2, a sua
    volta gli stessi utilizzati nel Curve Editor visto a lezione.

    L'editor implementa il double buffering: nessun flickering.

*)

open System.Windows.Forms
open System.Drawing
open System.Text.RegularExpressions
open libLWCs

type NavBut =
    | Up    = 0
    | Left  = 1
    | Down  = 2
    | Right = 3

type StateEditor =
    | FreeTransform = 0
    | InsertCircle  = 1
    | InsertRectang = 2
    | InsertLine    = 3
    | InsertBezier  = 4
    | MoveThings    = 5

// Controllo navigazione ------------------------------------------------------------------------------ //
type LWCNav(loc: PointF, siz: SizeF, mode: LWCmode, par: Control) =
    inherit LWC(loc, siz, mode, par)

    let mutable isSelected = false
    let notSelectedColor   = Color.FromArgb(0xCC, 0x00, 0x00)
    let selectedColor      = Color.FromArgb(0xB7, 0x00, 0x00)

    let clickEvt = new Event<System.EventArgs>()
    let upEvt    = new Event<MouseEventArgs>()
    let downEvt  = new Event<MouseEventArgs>()
    let moveEvt  = new Event<MouseEventArgs>()

    let mutable text = ""

    member this.Click     = clickEvt.Publish
    member this.MouseDown = downEvt.Publish
    member this.MouseUp   = upEvt.Publish

    member this.Text with get() = text and set(v) = text <- v

    override this.OnMouseUp e =
        upEvt.Trigger e
        clickEvt.Trigger (new System.EventArgs())
        // Aggiorna il controllo
        isSelected <- false
        this.Invalidate()

    override this.OnMouseDown e =
        downEvt.Trigger e
        // Aggiorna il controllo
        isSelected <- true
        this.Invalidate()

    override this.OnPaint e =
        let gCont = e.Graphics
        let bBrush = if isSelected then new SolidBrush(selectedColor) 
                     else new SolidBrush(notSelectedColor)
        let sBrush = new SolidBrush(Color.FromArgb(0x99, 0x99, 0x99))
        let bPen   = new Pen(Color.FromArgb(0xAA, 0xAA, 0xAA))
        // Spessore della penna
        bPen.Width <- 2.f
        // Contorno bottone
        gCont.DrawEllipse(bPen, this.PViewLoc.X, this.PViewLoc.Y, this.Size.Width, this.Size.Height)
        // Bottone
        gCont.FillEllipse(bBrush, this.PViewLoc.X, this.PViewLoc.Y, this.Size.Width, this.Size.Height)
        // Identificazione bottone
        gCont.DrawString(text, this.Parent.Font, sBrush, PointF(loc.X + 5.f, loc.Y + 5.f))
// ---------------------------------------------------------------------------------------------------- //

// Controllo LWC selettori editor --------------------------------------------------------------------- //
type LWCSelector(loc: PointF, siz: SizeF, mode: LWCmode, par: Control, nscolor: Color, scolor: Color) =
    inherit LWC(loc, siz, mode, par)

    let mutable isSelected = false
    let notSelectedColor   = nscolor
    let selectedColor      = scolor

    let mutable text = ""

    let clickEvt = new Event<System.EventArgs>()
    let upEvt    = new Event<MouseEventArgs>()
    let downEvt  = new Event<MouseEventArgs>()

    member this.Click     = clickEvt.Publish
    member this.MouseDown = downEvt.Publish

    member this.Text with get() = text and set(v) = text <- v

    override this.OnMouseUp e =
        upEvt.Trigger e
        clickEvt.Trigger (new System.EventArgs())
        // Aggiorna il controllo
        isSelected <- false
        this.Invalidate()
        base.OnMouseUp e

    override this.OnMouseDown e =
        downEvt.Trigger e
        // Aggiorna il controllo
        isSelected <- true
        this.Invalidate()
        base.OnMouseDown e

    override this.OnPaint e =
        let gCont = e.Graphics
        let bBrush = if isSelected then new SolidBrush(selectedColor) 
                     else new SolidBrush(notSelectedColor)
        let sBrush = new SolidBrush(Color.FromArgb(0x99, 0x99, 0x99))
        let bPen   = new Pen(Color.FromArgb(0xAA, 0xAA, 0xAA))
        // Spessore della penna
        bPen.Width <- 3.f
        // Contorno bottone
        gCont.DrawEllipse(bPen, this.PViewLoc.X, this.PViewLoc.Y, this.Size.Width, this.Size.Height)
        // Bottone
        gCont.FillEllipse(bBrush, this.PViewLoc.X, this.PViewLoc.Y, this.Size.Width, this.Size.Height)
        // Identificazione bottone
        gCont.DrawString(text, this.Parent.Font, sBrush, PointF(loc.X + 9.f, loc.Y + 24.f))
// ---------------------------------------------------------------------------------------------------- //

// ---------------------------------------------------------------------------------------------------- //
type VectorControl() as this =
    inherit LWCcontainer()

    let mutable (editState: StateEditor) = StateEditor.MoveThings

    // Oggetti da disegnare
    let objects = new ResizeArray<PointF[] * string * float32>()
    // Grandezza penna per la drawing
    let mutable sizePen = 1

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
    let TransformPW (m: Drawing2D.Matrix) (p: Point) =
        let point = [| PointF(single p.X, single p.Y) |]
        m.TransformPoints(point)
        point.[0]

    // Indica il vettore per lo scrolling (traslazione)
    let scrollBy dir =
        match dir with
        | NavBut.Up    -> (0.f, -10.f)
        | NavBut.Down  -> (0.f,  10.f)
        | NavBut.Left  -> (-10.f, 0.f)
        | NavBut.Right -> (10.f,  0.f)

    // Handle per i tasti premuti
    let handleKeys (k: Keys) =
        match k with
        // Traslazione
        | Keys.W -> scrollBy NavBut.Up    |> Translate; this.Invalidate()
        | Keys.A -> scrollBy NavBut.Left  |> Translate; this.Invalidate()
        | Keys.S -> scrollBy NavBut.Down  |> Translate; this.Invalidate()
        | Keys.D -> scrollBy NavBut.Right |> Translate; this.Invalidate()
        // Rotazione
        | Keys.Q -> let p = TransformPW this.V2W (Point(this.Width / 2, this.Height / 2))
                    RotateAtW p 10.f
                    this.Invalidate()
        | Keys.E -> let p = TransformPW this.V2W (Point(this.Width / 2, this.Height / 2))
                    RotateAtW p -10.f
                    this.Invalidate()
        // Scalatura
        | Keys.Z -> let pScale = TransformPW this.V2W (Point(this.Width / 2, this.Height / 2))
                    ScaleW(1.1f, 1.1f)
                    this.Invalidate()
        | Keys.X -> let pScale = TransformPW this.V2W (Point(this.Width / 2, this.Height / 2))
                    ScaleW(1.f/1.1f, 1.f/1.1f)
                    this.Invalidate()
        | _ -> ()

    // Creazione punti disegno
    let constructionPoints (ip: Point) (dp: Point) =
        // Ordina i punti e costruisce l'array associato
        let topPointX, botPointX = if ip.X > dp.X then single dp.X, single ip.X else single ip.X, single dp.X
        let topPointY, botPointY = if ip.Y > dp.Y then single dp.Y, single ip.Y else single ip.Y, single dp.Y

        [| PointF(topPointX, topPointY); PointF(botPointX, botPointY); 
           PointF(botPointX, topPointY); PointF(topPointX, botPointY) |]

    let constructionPointsF (ip: PointF) (dp: PointF) =
        // Utilizza i float
        let topPointX, botPointX = if ip.X > dp.X then dp.X, ip.X else ip.X, dp.X
        let topPointY, botPointY = if ip.Y > dp.Y then dp.Y, ip.Y else ip.Y, dp.Y

        [| PointF(topPointX, topPointY); PointF(botPointX, botPointY); 
           PointF(botPointX, topPointY); PointF(topPointX, botPointY) |]

    let constructionLine (ip: Point) (dp: Point) =
        // Costruisce la linea con i due punti
        [| PointF(single ip.X, single ip.Y); PointF(single dp.X, single dp.Y) |]

    let constructionLineF (ip: PointF) (dp: PointF) =
        // Utilizza i float
        [| PointF(ip.X, ip.Y); PointF(dp.X, dp.Y) |]

    // Timer scrolling
    let scrollTimer = new Timer(Interval = 30)
    // Direzione scrolling
    let mutable scrollDir = NavBut.Up
    do
        scrollTimer.Tick.Add(fun _ ->
            scrollBy scrollDir |> Translate
            this.Invalidate()
        )

    // Drag & drop
    let mutable (initPoint: Point option) = None
    let mutable (dragPoint: Point option) = None

    let addObject (s: string) =
        match initPoint, dragPoint with
        | Some ip, Some dp -> let points = if s = "line" then constructionLine ip dp else constructionPoints ip dp
                              this.V2W.TransformPoints(points)
                              objects.Add((points, s, single sizePen))
                              initPoint <- None
                              dragPoint <- None
        | _, _ -> ()
        editState <- StateEditor.MoveThings

    // Oggetto selezionato
    let mutable (idxSelected: int option)                 = None
    let mutable (objSelected: (PointF[] * string * float32) option) = None
    // Punti di riferimento per il drag&drop
    let mutable startDragging  = PointF()
    let mutable offsetDragging = PointF()

    let handleSize = 5.f
    // Drag&Drop disabilitato per le curve di Bezièr
    let handleHitTestM (point: PointF) (hit: PointF[] * string * float32) =
        match hit with
        | p, id, sz  -> if id = "line" then
                       // Costruisci un rettangolo per controllare se si trova all'interno di esso
                            let points = constructionPoints (Point(int p.[0].X, int p.[0].Y)) (Point(int p.[1].X, int p.[1].Y))
                            RectangleF(points.[0], SizeF(points.[1].X - points.[0].X, points.[1].Y - points.[0].Y)).Contains(point)
                        elif id = "bezi" then false
                        else RectangleF(p.[0], SizeF(p.[1].X - p.[0].X, p.[1].Y - p.[0].Y)).Contains(point)
        | _     -> false

    // Restituisce l'indice dello hit con l'handle di modifica (no hit = -1)
    let getIdxHit (point: PointF) (hit: PointF[]) =
        let mutable idxGot = -1
        for idx in { 0 .. (hit.Length - 1) } do
            let x = point.X - hit.[idx].X
            let y = point.Y - hit.[idx].Y
            if (x * x) + (y * y) <= (handleSize * handleSize) then idxGot <- idx
        idxGot

    // HitTest per l'edit
    let handleHitTestE (point: PointF) (hit: PointF[] * string * float32) =
        match hit with
        | p, id, sz -> if getIdxHit point p <> -1 then true else false
        | _         -> false

    let drawHandlesR (g: Graphics) (pts: PointF[]) = 
        // Il contesto grafico è già traslato su W2V
        for p in pts do
            g.FillEllipse(Brushes.Blue, p.X - handleSize, p.Y - handleSize, 2.f * handleSize, 2.f * handleSize)

    // Punto di inizio dragging
    let mutable idxPointer  = -1

    let movePointsBezi (newPt: PointF) (pts: PointF[]) =
        let mutable newPoint0 = pts.[0]
        let mutable newPoint1 = pts.[1]
        let mutable newPoint2 = pts.[2]
        let mutable newPoint3 = pts.[3]

        match idxPointer with
        // Ricalcolare i punti influenzati dal dragging
        | 0 -> newPoint0 <- PointF(newPt.X + offsetDragging.X, newPt.Y + offsetDragging.Y)
        | 1 -> newPoint1 <- PointF(newPt.X + offsetDragging.X, newPt.Y + offsetDragging.Y)
        | 2 -> newPoint2 <- PointF(newPt.X + offsetDragging.X, newPt.Y + offsetDragging.Y)
        | 3 -> newPoint3 <- PointF(newPt.X + offsetDragging.X, newPt.Y + offsetDragging.Y)

        [| newPoint0; newPoint1; newPoint2; newPoint3 |]

    let movePointsLine (newPt: PointF) (pts: PointF []) =
        let mutable newPoint0 = pts.[0]
        let mutable newPoint1 = pts.[1]

        match idxPointer with
        | 0 -> newPoint0 <- PointF(newPt.X + offsetDragging.X, newPt.Y + offsetDragging.Y)
        | 1 -> newPoint1 <- PointF(newPt.X + offsetDragging.X, newPt.Y + offsetDragging.Y)

        constructionLineF newPoint0 newPoint1

    // Muove i nuovi punti
    let movePoints (newPt: PointF) (pts: PointF[]) =
        let mutable newPoint0 = pts.[0]
        let mutable newPoint1 = pts.[1]
        let mutable newPoint2 = pts.[2]
        let mutable newPoint3 = pts.[3]

        match idxPointer with
        // Ricalcolare i punti influenzati dal dragging
        | 0 -> newPoint0 <- PointF(newPt.X + offsetDragging.X, newPt.Y + offsetDragging.Y)
               newPoint2 <- PointF(newPoint1.X, newPoint0.Y)
               newPoint3 <- PointF(newPoint0.X, newPoint1.Y)
        | 1 -> newPoint1 <- PointF(newPt.X + offsetDragging.X, newPt.Y + offsetDragging.Y)
               newPoint2 <- PointF(newPoint1.X, newPoint0.Y)
               newPoint3 <- PointF(newPoint0.X, newPoint1.Y)
        | 2 -> newPoint2 <- PointF(newPt.X + offsetDragging.X, newPt.Y + offsetDragging.Y)
               newPoint0 <- PointF(newPoint0.X, newPoint2.Y)
               newPoint1 <- PointF(newPoint2.X, newPoint1.Y)
        | 3 -> newPoint3 <- PointF(newPt.X + offsetDragging.X, newPt.Y + offsetDragging.Y)
               newPoint0 <- PointF(newPoint3.X, newPoint0.Y)
               newPoint1 <- PointF(newPoint1.X, newPoint3.Y)

        // Ricostruisce secondo l'ordine corretto il nuovo array di punti
        constructionPointsF newPoint0 newPoint1

    // Grandezza della penna
    member this.SizePen with get() = sizePen and set(v) = sizePen <- v

    // Pressione tasto navigazione Up
    member this.PressedUp   = scrollDir <- NavBut.Up; scrollTimer.Start()
    member this.UnPressedUp = scrollTimer.Stop()
    // Pressione tasto navigazione Down
    member this.PressedDown   = scrollDir <- NavBut.Down; scrollTimer.Start()
    member this.UnPressedDown = scrollTimer.Stop()
    // Pressione tasto navigazione Left
    member this.PressedLeft   = scrollDir <- NavBut.Left; scrollTimer.Start()
    member this.UnPressedLeft = scrollTimer.Stop()
    // Pressione tasto navigazione Right
    member this.PressedRight   = scrollDir <- NavBut.Right; scrollTimer.Start()
    member this.UnPressedRight = scrollTimer.Stop()

    // Echo dell'evento KeyDown dal DockControl
    member this.DockKeyDown e =
        this.OnKeyDown e

    // Stato interno dell'editor
    member this.EditorState with get() = editState and set(v) = editState <- v
    member this.Objects = objects

    override this.OnKeyDown e =
        handleKeys e.KeyCode

    override this.OnMouseDown e =
        match editState with
        | StateEditor.MoveThings                                       ->
            // Trasforma le coordinate del mouse in coordinate mondo
            let mPoint  = TransformPW this.V2W e.Location
            let hitTest = handleHitTestM mPoint
            // Trova elementi che corrispondono al click
            idxSelected <- objects |> Seq.tryFindIndexBack hitTest
            (match idxSelected with
            // Segna l'elemento trovato come elemento selezionato e rimuovi dalla collezione
            | Some v -> objSelected <- Some objects.[v]; objects.RemoveAt(v)
            | None   -> ())
            // Se premo tasto sinistro, drag&drop, destro elimina l'oggetto
            if e.Button = MouseButtons.Left then
                (match objSelected with
                | Some (p, s, sz) -> offsetDragging <- PointF(p.[0].X - mPoint.X, p.[0].Y - mPoint.Y)
                | None            -> this.Focus() |> ignore)
        | StateEditor.FreeTransform when e.Button = MouseButtons.Left  -> 
            // Trasforma le coordinate del mouse in coordinate mondo
            let mPoint  = TransformPW this.V2W e.Location
            let hitTest = handleHitTestE mPoint
            // Trova elementi che corrispondono al click
            idxSelected <- objects |> Seq.tryFindIndexBack hitTest
            (match idxSelected with
            // Segna l'elemento trovato come elemento selezionato e rimuovi dalla collezione
            | Some v -> objSelected <- Some objects.[v]; objects.RemoveAt(v)
            | None   -> ())
            // Calcola l'offset per il dragging
            (match objSelected with
            | Some (p, s, sz) -> idxPointer     <- getIdxHit mPoint p
                                 offsetDragging <- PointF(p.[idxPointer].X - mPoint.X, p.[idxPointer].Y - mPoint.Y) 
            | None            -> ())
        | _                         when e.Button = MouseButtons.Left  -> initPoint <- Some (e.Location)
        | _                         when e.Button = MouseButtons.Right -> editState <- StateEditor.MoveThings
        | _                                                            -> ()

    override this.OnMouseMove e =
        match editState with
        | StateEditor.MoveThings    when e.Button = MouseButtons.Left  -> 
            (match objSelected with
            | Some (p, s, sz) -> let newPointer = TransformPW this.V2W e.Location
                                 // Lunghezza e larghezza vengono preservate col dragging
                                 let width, height = p.[1].X - p.[0].X, p.[1].Y - p.[0].Y
                                 let newPoints  =
                                    // Nel caso della linea, ricalcola i due nuovi punti
                                    if s = "line" then [| PointF(newPointer.X + offsetDragging.X, newPointer.Y + offsetDragging.Y);
                                                          PointF(width + newPointer.X + offsetDragging.X, height + newPointer.Y + offsetDragging.Y) |]
                                    // Ricalcola tutti i nuovi punti
                                    else [| PointF(newPointer.X + offsetDragging.X, newPointer.Y + offsetDragging.Y);
                                            PointF(width + newPointer.X + offsetDragging.X, height + newPointer.Y + offsetDragging.Y);
                                            PointF(width + newPointer.X + offsetDragging.X, newPointer.Y + offsetDragging.Y);
                                            PointF(newPointer.X + offsetDragging.X, height + newPointer.Y + offsetDragging.Y) |]
                                 // Oggetto aggiornato
                                 objSelected <- Some (newPoints, s, sz)
            | None        -> ())
        | StateEditor.MoveThings    when e.Button = MouseButtons.Right -> ()
        | StateEditor.FreeTransform when e.Button = MouseButtons.Left  -> 
            (match objSelected with
            | Some (p, s, sz) -> let newPointer = TransformPW this.V2W e.Location
                                 if   s = "line" then objSelected <- Some (movePointsLine newPointer p, s, sz)
                                 elif s = "bezi" then objSelected <- Some (movePointsBezi newPointer p, s, sz)
                                 else                 objSelected <- Some (movePoints newPointer p, s, sz)
            | None        -> ())
        | StateEditor.FreeTransform when e.Button = MouseButtons.Right -> ()
        | _                                                            -> dragPoint <- Some (e.Location)
        this.Invalidate()

    override this.OnMouseUp e =
        match editState with
        | StateEditor.MoveThings    ->
            (match objSelected with
            | Some v -> if e.Button = MouseButtons.Left then objects.Add(v)
                        idxSelected <- None; objSelected <- None
            | None   -> ())
        | StateEditor.FreeTransform -> 
            (match objSelected with
            | Some v -> if e.Button = MouseButtons.Left then objects.Add(v)
                        idxSelected <- None; objSelected <- None; idxPointer <- -1
            | None   -> ())
        // Aggiungi il rettangolo
        | StateEditor.InsertRectang -> addObject "rect"
        // Aggiungi il cerchio
        | StateEditor.InsertCircle  -> addObject "circ"
        // Aggiungi la linea
        | StateEditor.InsertLine    -> addObject "line"
        // Aggiungi la curva di Bezièr
        | StateEditor.InsertBezier  -> addObject "bezi"
        // Aggiorna la vista
        this.Invalidate()
    
    override this.UseBuffer() =
        this.UpdateBuffer()
        let gBCont  = Graphics.FromImage(this.Buffer)
        let bgBrush = new SolidBrush(Color.FromArgb(0xA7, 0xA7, 0xA7))
        gBCont.FillRectangle(bgBrush, 0, 0, this.Buffer.Width, this.Buffer.Height)
        gBCont

    override this.OnPaint e =
        let gCont = this.UseBuffer()
        let gSave = gCont.Save()
        let drawIPen = new Pen(Color.Red, single sizePen)

        // Disegno roba intermedia
        match initPoint, dragPoint with
        | Some ip, Some dp -> gCont.DrawLine(drawIPen, ip, dp)
        | _, _ -> ()

        gCont.Transform <- this.W2V
        // Disegna gli oggetti
        objects |> Seq.iter (fun obj ->
            match obj with
            | p, id, sz when id = "rect" -> gCont.FillRectangle(Brushes.Red, p.[0].X, p.[0].Y, p.[1].X - p.[0].X, p.[1].Y - p.[0].Y)
                                            if editState = StateEditor.FreeTransform then drawHandlesR gCont p
            | p, id, sz when id = "circ" -> gCont.FillEllipse(Brushes.Red, p.[0].X, p.[0].Y, p.[1].X - p.[0].X, p.[1].Y - p.[0].Y)
                                            if editState = StateEditor.FreeTransform then drawHandlesR gCont p
            | p, id, sz when id = "line" -> let drawPen = new Pen(Color.Black, sz)
                                            gCont.DrawLine(drawPen, p.[0], p.[1])
                                            if editState = StateEditor.FreeTransform then drawHandlesR gCont p
            | p, id, sz when id = "bezi" -> let drawPen = new Pen(Color.Black, sz)
                                            gCont.DrawBezier(drawPen, p.[0], p.[2], p.[1], p.[3])
                                            if editState = StateEditor.FreeTransform then drawHandlesR gCont p
            | _ -> ()
        )

        // Disegna dragging
        match objSelected with
        | Some (p, id, sz) when id = "rect" -> gCont.FillRectangle(Brushes.Red, p.[0].X, p.[0].Y, p.[1].X - p.[0].X, p.[1].Y - p.[0].Y)
                                               if editState = StateEditor.FreeTransform then drawHandlesR gCont p
        | Some (p, id, sz) when id = "circ" -> gCont.FillEllipse(Brushes.Red, p.[0].X, p.[0].Y, p.[1].X - p.[0].X, p.[1].Y - p.[0].Y)
                                               if editState = StateEditor.FreeTransform then drawHandlesR gCont p
        | Some (p, id, sz) when id = "line" -> let drawPen = new Pen(Color.Black, sz)
                                               gCont.DrawLine(drawPen, p.[0], p.[1])
                                               if editState = StateEditor.FreeTransform then drawHandlesR gCont p
        | Some (p, id, sz) when id = "bezi" -> let drawPen = new Pen(Color.Black, sz)
                                               gCont.DrawBezier(drawPen, p.[0], p.[2], p.[1], p.[3])
                                               if editState = StateEditor.FreeTransform then drawHandlesR gCont p
        | _ -> ()

        gCont.Restore(gSave)
        base.OnPaint e
// ---------------------------------------------------------------------------------------------------- //

type myTextBox() as this =
    inherit TextBox()

    do
        this.Location <- Point(20, 480)
        this.Size     <- Size(60, 20)
        this.Text     <- "1"

    override this.OnTextChanged e =
        if this.Text = "" then this.Text <- "1"
        base.OnTextChanged e

// Dock per i tasti comando --------------------------------------------------------------------------- //
type DockControl(vectsheet: VectorControl) as this =
    inherit LWCcontainer()

    // Bottoni per aggiungere primitive grafiche allo spazio vettoriale
    let buttons = [| new LWCSelector(PointF(20.f, 100.f), SizeF(60.f, 60.f), LWCmode.ViewMode, this, Color.FromArgb(0xEC, 0xE4, 0xD4), Color.FromArgb(0xD4, 0xCD, 0xBE));
                     new LWCSelector(PointF(20.f, 180.f), SizeF(60.f, 60.f), LWCmode.ViewMode, this, Color.FromArgb(0xBA, 0xDB, 0xAD), Color.FromArgb(0xA7, 0xC5, 0x9B));
                     new LWCSelector(PointF(20.f, 260.f), SizeF(60.f, 60.f), LWCmode.ViewMode, this, Color.FromArgb(0xC6, 0xE0, 0xFF), Color.FromArgb(0xB2, 0xC9, 0xE5));
                     new LWCSelector(PointF(20.f, 340.f), SizeF(60.f, 60.f), LWCmode.ViewMode, this, Color.FromArgb(0xB3, 0x78, 0x85), Color.FromArgb(0xA1, 0x6C, 0x77));
                     new LWCSelector(PointF(30.f, 420.f), SizeF(40.f, 40.f), LWCmode.ViewMode, this, Color.FromArgb(0x46, 0x46, 0xDA), Color.FromArgb(0x3F, 0x3F, 0xC4)) |]
    // Bottoni di navigazione
    let navbuts = [| new LWCNav(PointF(38.f, 20.f), SizeF(20.f, 20.f), LWCmode.ViewMode, this, Text = "U");
                     new LWCNav(PointF(18.f, 40.f), SizeF(20.f, 20.f), LWCmode.ViewMode, this, Text = "L");
                     new LWCNav(PointF(38.f, 60.f), SizeF(20.f, 20.f), LWCmode.ViewMode, this, Text = "D");
                     new LWCNav(PointF(58.f, 40.f), SizeF(20.f, 20.f), LWCmode.ViewMode, this, Text = "R"); |]

    let textBox = new myTextBox()

    do
        // Imposta la larghezza della dock
        this.Width <- 100
        // Inserisci la textbox
        this.Controls.Add(textBox)

        // Inserisci i bottoni nella dock
        buttons |> Array.iter (fun b -> this.LWControls.Add(b))
        navbuts |> Array.iter (fun b -> this.LWControls.Add(b))

        buttons.[0].Text <- "Ellipse"
        buttons.[1].Text <- "Rectangle"
        buttons.[2].Text <- "Line"
        buttons.[3].Text <- "Bezièr"
        buttons.[4].Text <- "ED"

        // Registra gli eventi per i tasti editor
        buttons.[0].MouseDown.Add(fun _ -> vectsheet.EditorState <- StateEditor.InsertCircle)
        buttons.[1].MouseDown.Add(fun _ -> vectsheet.EditorState <- StateEditor.InsertRectang)
        buttons.[2].MouseDown.Add(fun _ ->
            let gotFromTBox = textBox.Text
            let intFromTBox = System.Int32.Parse(gotFromTBox)
            if intFromTBox > 0 then vectsheet.SizePen <- intFromTBox
            else vectsheet.SizePen <- 1
            vectsheet.EditorState <- StateEditor.InsertLine
        )
        buttons.[3].MouseDown.Add(fun _ -> 
            let gotFromTBox = textBox.Text
            let intFromTBox = System.Int32.Parse(gotFromTBox)
            if intFromTBox > 0 then vectsheet.SizePen <- intFromTBox
            else vectsheet.SizePen <- 1
            vectsheet.EditorState <- StateEditor.InsertBezier
        )
        buttons.[4].MouseDown.Add(fun _ ->
            if vectsheet.EditorState = StateEditor.FreeTransform then
                vectsheet.EditorState <- StateEditor.MoveThings
            else
                vectsheet.EditorState <- StateEditor.FreeTransform
            vectsheet.Invalidate()
        )

        // Registra gli eventi pressione tasti navigazione
        navbuts.[int(NavBut.Up)].MouseDown.Add(fun _ -> vectsheet.PressedUp)
        navbuts.[int(NavBut.Up)].MouseUp.Add(fun _ -> vectsheet.UnPressedUp)

        navbuts.[int(NavBut.Down)].MouseDown.Add(fun _ -> vectsheet.PressedDown)
        navbuts.[int(NavBut.Down)].MouseUp.Add(fun _ -> vectsheet.UnPressedDown)

        navbuts.[int(NavBut.Left)].MouseDown.Add(fun _ -> vectsheet.PressedLeft)
        navbuts.[int(NavBut.Left)].MouseUp.Add(fun _ -> vectsheet.UnPressedLeft)

        navbuts.[int(NavBut.Right)].MouseDown.Add(fun _ -> vectsheet.PressedRight)
        navbuts.[int(NavBut.Right)].MouseUp.Add(fun _ -> vectsheet.UnPressedRight)

    override this.OnMouseDown e =
        base.OnMouseDown e

    override this.OnMouseUp e =
        base.OnMouseUp e

    override this.OnKeyDown e =
        vectsheet.DockKeyDown e
    
    // Override necessario per cambiare le impostazioni dello sfondo
    override this.UseBuffer() =
        this.UpdateBuffer()
        let gBCont  = Graphics.FromImage(this.Buffer)
        let bgBrush = new SolidBrush(Color.FromArgb(0x97, 0x97, 0x97))
        gBCont.FillRectangle(bgBrush, 0, 0, this.Buffer.Width, this.Buffer.Height)
        gBCont

    override this.OnPaint e =
        let buf = this.UseBuffer()
        base.OnPaint e
// ---------------------------------------------------------------------------------------------------- //

let myForm = new Form(Text = "Esercizio 3/4 - IUM Midterm", TopMost = true)
let vectForm = new VectorControl(Dock = DockStyle.Fill)
let dockForm = new DockControl(vectForm, Dock = DockStyle.Left)

// Grandezza minima della form
myForm.MinimumSize <- Size(860, 540)
myForm.Controls.Add(vectForm)
myForm.Controls.Add(dockForm)
myForm.Show()