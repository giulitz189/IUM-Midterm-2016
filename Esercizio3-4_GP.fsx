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

        N.B. il drag&drop e l'eliminazione non sono supportate per le curve di Bezièr in MoveThings
             (ho avuto problemi con lo hitTest), solo l'eliminazione in FreeTransform tramite tasto R.

    Nello stato MoveThings, l'editor permette il drag&drop col tasto sinistro del mouse,
    e l'eliminazione di una primitiva col tasto destro.

    Nello stato FreeTransform, l'editor permette di modificare i punti di disegno delle
    primitive col tasto sinistro sull'handler; selezionando il tasto ED dal dock in questo stato,
    permette di tornare allo stato di default, MoveThings.

    Con il WASD è possibile muovere la primitiva grafica per ultimo selezionata (tramite handlers),
    mentre con la R è rimuovibile.

    I comandi da tastiera sono gli stessi utilizzati nel Curve Editor dell'Esercizio 2, a sua
    volta gli stessi utilizzati nel Curve Editor visto a lezione.

    L'editor implementa il double buffering: nessun flickering.

*)

open System.Windows.Forms
open System.Drawing
open System.Text.RegularExpressions
open libLWCs

type NavBut =
    | Up     = 0
    | Left   = 1
    | Down   = 2
    | Right  = 3
    | RLeft  = 4
    | RRight = 5

type StateEditor =
    | EditMode      = 0
    | InsertCircle  = 1
    | InsertRectang = 2
    | InsertLine    = 3
    | InsertBezier  = 4
    | InsertBall    = 5
    | DefaultState  = 6

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
type GraphicsObjectType =
    | Ellipse   = 0
    | Rectangle = 1
    | Line      = 2
    | Bezier    = 3
// ---------------------------------------------------------------------------------------------------- //
// ---------------------------------------------------------------------------------------------------- //
type GraphicsObject(points: PointF[], sizePen: float32, typ: GraphicsObjectType) =
    let mutable graphicsPath = new Drawing2D.GraphicsPath()
    let mutable sPoints = points

    let getRectangleF (points: PointF[]) =
        let x, y = points.[0].X, points.[0].Y
        let width, height = points.[1].X - x, points.[1].Y - y
        new RectangleF(x, y, width, height)

    let initializeGPathB (points: PointF[]) =
        graphicsPath.AddBezier(points.[0], points.[2], points.[1], points.[3])

    let initializeGPath (rect: RectangleF) =
        match typ with
        | GraphicsObjectType.Ellipse   -> graphicsPath.AddEllipse(rect)
        | GraphicsObjectType.Rectangle -> graphicsPath.AddRectangle(rect)
        | GraphicsObjectType.Line      -> graphicsPath.AddLine(rect.Location, PointF(rect.Right, rect.Bottom))
        | GraphicsObjectType.Bezier    -> ()

    let constructPath (points: PointF[]) =
        if typ = GraphicsObjectType.Bezier then initializeGPathB points
        else initializeGPath (getRectangleF points)

    do constructPath sPoints

    member this.Type         = typ
    member this.SizePen      = sizePen
    member this.GraphicsPath = graphicsPath
    member this.GetRegion    = new Region(graphicsPath)
    // Array di punti handlers
    member this.Handlers 
        with get() = sPoints 
        and set(v) = sPoints <- v; graphicsPath.Dispose(); 
                     graphicsPath <- new Drawing2D.GraphicsPath()
                     constructPath sPoints

    member this.TranslateTo (p: PointF) =
        // Punti correnti della primitiva
        let transformationMatrix = new Drawing2D.Matrix()
        // Trasla la matrice
        transformationMatrix.Translate(p.X, p.Y)
        transformationMatrix.TransformPoints(sPoints)
        // Elimina il vecchio graphicsPath e creane uno nuovo
        graphicsPath.Dispose()
        graphicsPath <- new Drawing2D.GraphicsPath()
        constructPath sPoints
// ---------------------------------------------------------------------------------------------------- //
// ---------------------------------------------------------------------------------------------------- //
type Ball(loc: PointF, spd: SizeF, parent: LWCcontainer) =
    let size = SizeF(25.f, 25.f)
    let ballPen   = new Pen(Color.DarkBlue)
    let ballBrush = new SolidBrush(Color.Blue)
    
    let mutable location = loc
    let mutable speed    = spd
    let mutable lastT    = System.DateTime.Now

    let collideWith (obj: GraphicsObject) =
        let gPBall = new Drawing2D.GraphicsPath()
        gPBall.AddEllipse(location.X, location.Y, size.Width, size.Height)
        let ballRegion = new Region(gPBall)
        ballRegion.Intersect(obj.GetRegion)

    member this.Location = location
    member this.Speed    = speed
    member this.Bounds   = new RectangleF(location, size)
    member this.BPen     = ballPen
    member this.BBrush   = ballBrush

    member this.UpdatePosition =
        let t  = System.DateTime.Now
        let dt = single (t - lastT).TotalSeconds
        let vx = speed.Width / 2.f
        let vy = speed.Height / 2.f
        let x = - vx * dt
        let y = - (vy * dt) + ((5.f * (dt*dt)) / 2.f)
        location <- new PointF(location.X + x, location.Y + y)
    
// ---------------------------------------------------------------------------------------------------- //
// ---------------------------------------------------------------------------------------------------- //
// Specifica il tipo di selezione in un drag&drop
type Selection =
    | Handlers = 0
    | Graphics = 1
    | Nothing  = 2

// ---------------------------------------------------------------------------------------------------- //
type VectorControl() as this =
    inherit LWCcontainer()

    let mutable (editState: StateEditor) = StateEditor.DefaultState

    // Oggetti da disegnare
    let objects = new ResizeArray<GraphicsObject>()
    let balls   = new ResizeArray<Ball>()
    // Grandezza penna per la drawing
    let mutable sizePen = 1.f

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
    let RotateAtW (point, angle) =
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

    // Vettore per la rotazione
    let rotateBy dir =
        let p = TransformPW this.V2W (Point(this.Width / 2, this.Height / 2))
        match dir with
        | NavBut.RLeft  -> (p, -10.f)
        | NavBut.RRight -> (p, 10.f)

    // Oggetto selezionato
    let mutable (idxSelected: int option)            = None
    let mutable (objSelected: GraphicsObject option) = None
    // Punti di riferimento per il drag&drop
    let mutable startDragging  = PointF()
    let mutable offsetDragging = PointF()

    // Punto di inizio dragging
    let mutable idxPointer  = -1

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

    // Trasla una primitiva grafica selezionata in edit mode
    let editTranslateTo (key: Keys) =
        let obj = objects.[objects.Count - 1]
        let mutable vectorTranslation = PointF(0.f, 0.f)

        (match key with
         | Keys.W -> vectorTranslation <- PointF(0.f, -10.f)
         | Keys.A -> vectorTranslation <- PointF(-10.f, 0.f)
         | Keys.S -> vectorTranslation <- PointF(0.f, 10.f)
         | Keys.D -> vectorTranslation <- PointF(10.f, 0.f)
         | _      -> ())

        obj.TranslateTo vectorTranslation
            
    // Stringe/allarga una primitiva grafica selezionata in edit mode
    let editScaleTo (k: Keys) =
        ()

    // Handle per i tasti premuti
    let handleKeys (k: Keys) =
        match k with
        // Traslazione
        | Keys.W -> if editState = StateEditor.DefaultState then scrollBy NavBut.Up |> Translate
                    // Muove l'elemento selezionato in edit mode in su
                    elif editState = StateEditor.EditMode then
                        editTranslateTo k
                    this.Invalidate()
        | Keys.A -> if editState = StateEditor.DefaultState then scrollBy NavBut.Left |> Translate
                    // Muove l'elemento selezionato in edit mode a sinistra
                    elif editState = StateEditor.EditMode then
                        editTranslateTo k
                    this.Invalidate()
        | Keys.S -> if editState = StateEditor.DefaultState then scrollBy NavBut.Down |> Translate
                    // Muove l'elemento selezionato in edit mode in basso
                    elif editState = StateEditor.EditMode then
                        editTranslateTo k
                    this.Invalidate()
        | Keys.D -> if editState = StateEditor.DefaultState then scrollBy NavBut.Right |> Translate
                    // Muove l'elemento selezionato in edit mode a destra
                    elif editState = StateEditor.EditMode then
                        editTranslateTo k
                    this.Invalidate()
        // Rotazione
        | Keys.Q -> if editState = StateEditor.EditMode then ()
                    else RotateAtW (rotateBy NavBut.RLeft)
                    this.Invalidate()
        | Keys.E -> if editState = StateEditor.EditMode then ()
                    else RotateAtW (rotateBy NavBut.RRight)
                    this.Invalidate()
        // Scalatura
        | Keys.Z -> if editState = StateEditor.EditMode then () // editScaleTo k
                    else
                        let pScale = TransformPW this.V2W (Point(this.Width / 2, this.Height / 2))
                        ScaleW(1.1f, 1.1f)
                    this.Invalidate()
        | Keys.X -> if editState = StateEditor.EditMode then () // editScaleTo k
                    else
                        let pScale = TransformPW this.V2W (Point(this.Width / 2, this.Height / 2))
                        ScaleW(1.f/1.1f, 1.f/1.1f)
                    this.Invalidate()
        // Rimuove in edit mode
        | Keys.R when editState = StateEditor.EditMode ->
            (match objSelected with
            | None   -> ()
            | Some _ -> objSelected <- None; idxSelected <- None; idxPointer <- -1; this.Invalidate())
        | _ -> ()

    // Timer scrolling
    let scrollTimer = new Timer(Interval = 30)
    let rotateTimer = new Timer(Interval = 60)
    let ballsTimer  = new Timer(Interval = 17)
    // Direzione scrolling
    let mutable scrollDir = NavBut.Up
    let mutable rotateDir = NavBut.RLeft
    do
        scrollTimer.Tick.Add(fun _ ->
            scrollBy scrollDir |> Translate
            this.Invalidate()
        )

        rotateTimer.Tick.Add(fun _ ->
            rotateBy rotateDir |> RotateAtW
            this.Invalidate()
        )

        ballsTimer.Tick.Add(fun _ ->
            balls |> Seq.iter (fun b ->
                b.UpdatePosition
                this.Invalidate()
            )
        )
        ballsTimer.Start()

    // Drag & drop
    let mutable (initPoint: Point option) = None
    let mutable (dragPoint: Point option) = None

    let addObject (typ: GraphicsObjectType) =
        match initPoint, dragPoint with
        | Some ip, Some dp -> let points = if typ = GraphicsObjectType.Line then constructionLine ip dp
                                                                            else constructionPoints ip dp
                              this.V2W.TransformPoints(points)
                              objects.Add(new GraphicsObject(points, sizePen, typ))
                              initPoint <- None
                              dragPoint <- None
        | _, _ -> ()
        editState <- StateEditor.DefaultState

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

    let handleSize = 5.f
    // Test per drag&drop sulle primitive
    let handleHitTestG (point: PointF) (hit: GraphicsObject) =
        hit.GraphicsPath.GetBounds().Contains(point)

    // Restituisce l'indice dello hit con l'handle di modifica (no hit = -1)
    let getIdxHit (point: PointF) (hit: PointF[]) =
        let mutable idxGot = -1
        for idx in { 0 .. (hit.Length - 1) } do
            let x = point.X - hit.[idx].X
            let y = point.Y - hit.[idx].Y
            if (x * x) + (y * y) <= (handleSize * handleSize) then idxGot <- idx
        idxGot

    // HitTest per l'edit
    let handleHitTestH (point: PointF) (hit: GraphicsObject) =
        let gObjPoints = hit.Handlers
        if getIdxHit point gObjPoints <> -1 then true 
        else false

    let drawHandlesR (g: Graphics) (gObj: GraphicsObject) = 
        // Il contesto grafico è già traslato su W2V
        for p in gObj.Handlers do
            g.FillEllipse(Brushes.Blue, p.X - handleSize, p.Y - handleSize, 2.f * handleSize, 2.f * handleSize)

    // Stato del mouse negli eventi MouseDown, MouseMove e MouseUp
    let mutable mouseState = Selection.Nothing
    // Gestisce l'evento MouseDown in edit mode
    let managingMouseDown (e: MouseEventArgs) =
        let mPoint   = TransformPW this.V2W e.Location
        let hitTestH = handleHitTestH mPoint
        let hitTestG = handleHitTestG mPoint

        idxSelected <- objects |> Seq.tryFindIndexBack hitTestH

        if idxSelected.IsSome then mouseState <- Selection.Handlers
        else 
            idxSelected <- objects |> Seq.tryFindIndexBack hitTestG
            if idxSelected.IsSome then mouseState <- Selection.Graphics
       
        match idxSelected with
        | None   -> this.Focus() |> ignore
        | Some v -> objSelected <- Some objects.[v]; objects.RemoveAt(v)
                    // Segna l'offset per il drag dell'handler
                    if mouseState = Selection.Handlers then
                        let p = objSelected.Value.Handlers
                        idxPointer     <- getIdxHit mPoint p
                        offsetDragging <- PointF(p.[idxPointer].X - mPoint.X, p.[idxPointer].Y - mPoint.Y)
                    // Segna il punto di inizio del dragging della figura
                    elif mouseState = Selection.Graphics then
                        startDragging <- mPoint

    // Gestisce l'evento MouseMove in edit mode
    let managingMouseMove (e: MouseEventArgs) =
        let mPoint = TransformPW this.V2W e.Location

        match objSelected with
        | None     -> ()
        | Some obj when mouseState = Selection.Graphics ->
            offsetDragging <- PointF(mPoint.X - startDragging.X, mPoint.Y - startDragging.Y)
            startDragging  <- mPoint
            obj.TranslateTo offsetDragging
        | Some obj when mouseState = Selection.Handlers ->
            let oldPts = obj.Handlers
            if   obj.Type = GraphicsObjectType.Line   then obj.Handlers <- movePointsLine mPoint oldPts
            elif obj.Type = GraphicsObjectType.Bezier then obj.Handlers <- movePointsBezi mPoint oldPts
            else obj.Handlers <- movePoints mPoint oldPts
        | _ -> ()

    let managingMouseUp (e: MouseEventArgs) =
        match objSelected with
        | None -> this.Focus |> ignore
        | Some obj when e.Button = MouseButtons.Right   -> objSelected <- None; idxSelected <- None
        | Some obj when mouseState = Selection.Graphics -> if objSelected.IsSome then this.ResetState
        | Some obj when mouseState = Selection.Handlers -> if objSelected.IsSome then this.ResetState; idxPointer <- -1
        | _ -> ()

        mouseState <- Selection.Nothing
        
    let addBall() =
        match initPoint, dragPoint with
        | Some ip, Some dp ->
            let ipF, dpF  = PointF(single ip.X, single ip.Y), PointF(single dp.X, single dp.Y)
            let dpForTest = TransformPW this.V2W dp 
            let mutable invalidStart = false

            objects |> Seq.iter (fun obj ->
                invalidStart <- invalidStart || handleHitTestG dpForTest obj
            )

            if not invalidStart then
                let speed = SizeF(dpF.X - ipF.X, dpF.Y - ipF.Y)
                balls.Add(new Ball(dpF, speed, this))

            initPoint <- None; dragPoint <- None

        | _, _ -> ()
        editState <- StateEditor.DefaultState

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
    // Pressione tasto navigazione RLeft
    member this.PressedRLeft   = rotateDir <- NavBut.RLeft; rotateTimer.Start()
    member this.UnPressedRLeft = rotateTimer.Stop()
    // Pressione tasto navigazione RRight
    member this.PressedRRight   = rotateDir <- NavBut.RRight; rotateTimer.Start()
    member this.UnPressedRRight = rotateTimer.Stop()

    // Stato interno dell'editor
    member this.EditorState with get() = editState and set(v) = editState <- v
    member this.Objects = objects

    member this.ResetState =
        if objSelected.IsSome then
            objects.Add(objSelected.Value)
            objSelected <- None
            idxSelected <- None

    override this.OnKeyDown e =
        handleKeys e.KeyCode

    override this.OnMouseDown e =
        match editState with
        | StateEditor.DefaultState                                     -> this.Focus() |> ignore
        | StateEditor.EditMode                                         -> managingMouseDown e
        | _                         when e.Button = MouseButtons.Left  -> initPoint <- Some (e.Location)
        | _                         when e.Button = MouseButtons.Right -> editState <- StateEditor.DefaultState
        | _                                                            -> ()

    override this.OnMouseMove e =
        match editState with
        | StateEditor.DefaultState                                     -> ()
        | StateEditor.EditMode      when e.Button = MouseButtons.Right -> ()
        | StateEditor.EditMode      when e.Button = MouseButtons.Left  -> managingMouseMove e
        | _                                                            -> dragPoint <- Some (e.Location)
        this.Invalidate()

    override this.OnMouseUp e =
        match editState with
        | StateEditor.DefaultState  -> ()
        | StateEditor.EditMode      -> managingMouseUp e
        // Aggiungi il rettangolo
        | StateEditor.InsertRectang -> addObject GraphicsObjectType.Rectangle
        // Aggiungi il cerchio
        | StateEditor.InsertCircle  -> addObject GraphicsObjectType.Ellipse
        // Aggiungi la linea
        | StateEditor.InsertLine    -> addObject GraphicsObjectType.Line
        // Aggiungi la curva di Bezièr
        | StateEditor.InsertBezier  -> addObject GraphicsObjectType.Bezier
        | StateEditor.InsertBall    -> addBall()
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
        let drawIPen = new Pen(Color.Green, sizePen)
        let backIPen = new Pen(Color.FromArgb(0xB3, 0xB3, 0xB3))

        for i in { 0 .. 20 .. this.Width } do
            gCont.DrawLine(backIPen, Point(i, 0), Point(i, this.Height))

        for i in { 0 .. 20 .. this.Height } do
            gCont.DrawLine(backIPen, Point(0, i), Point(this.Width, i))

        gCont.Transform <- this.W2V
        // Disegna gli oggetti
        objects |> Seq.iter (fun obj ->
            if obj.Type = GraphicsObjectType.Line || obj.Type = GraphicsObjectType.Bezier then
                let myPen = new Pen(Color.Black, this.SizePen)
                gCont.DrawPath(myPen, obj.GraphicsPath)
            else
                gCont.FillPath(Brushes.Red, obj.GraphicsPath)

            if editState = StateEditor.EditMode then drawHandlesR gCont obj
        )

        // Disegna dragging
        match objSelected with
        | Some obj ->
            if obj.Type = GraphicsObjectType.Line || obj.Type = GraphicsObjectType.Bezier then
                let myPen = new Pen(Color.Black, this.SizePen)
                gCont.DrawPath(myPen, obj.GraphicsPath)
            else
                gCont.FillPath(Brushes.Red, obj.GraphicsPath)

            if editState = StateEditor.EditMode then drawHandlesR gCont obj
        | None -> ()

        gCont.Restore(gSave)

        balls |> Seq.iter (fun b ->
            gCont.FillEllipse(b.BBrush, b.Bounds)
            gCont.DrawEllipse(b.BPen, b.Bounds)
        )

        // Disegno roba intermedia
        match initPoint, dragPoint with
        | Some ip, Some dp -> gCont.DrawLine(drawIPen, ip, dp)
        | _, _ -> ()

        base.OnPaint e
// ---------------------------------------------------------------------------------------------------- //

// Fantastica TextBox per la size della pen ----------------------------------------------------------- //
type myTextBox() as this =
    inherit TextBox()

    do
        this.Location <- Point(20, 560)
        this.Size     <- Size(60, 20)
        this.Text     <- "1"

    override this.OnTextChanged e =
        if this.Text = "" then this.Text <- "1.0"
        base.OnTextChanged e
// ---------------------------------------------------------------------------------------------------- //

// Dock per i tasti comando --------------------------------------------------------------------------- //
type DockControl(vectsheet: VectorControl) as this =
    inherit LWCcontainer()

    // Bottoni per aggiungere primitive grafiche allo spazio vettoriale
    let buttons = [| new LWCSelector(PointF(20.f, 100.f), SizeF(60.f, 60.f), LWCmode.ViewMode, this, Color.FromArgb(0xEC, 0xE4, 0xD4), Color.FromArgb(0xD4, 0xCD, 0xBE));
                     new LWCSelector(PointF(20.f, 180.f), SizeF(60.f, 60.f), LWCmode.ViewMode, this, Color.FromArgb(0xBA, 0xDB, 0xAD), Color.FromArgb(0xA7, 0xC5, 0x9B));
                     new LWCSelector(PointF(20.f, 260.f), SizeF(60.f, 60.f), LWCmode.ViewMode, this, Color.FromArgb(0xC6, 0xE0, 0xFF), Color.FromArgb(0xB2, 0xC9, 0xE5));
                     new LWCSelector(PointF(20.f, 340.f), SizeF(60.f, 60.f), LWCmode.ViewMode, this, Color.FromArgb(0xB3, 0x78, 0x85), Color.FromArgb(0xA1, 0x6C, 0x77));
                     new LWCSelector(PointF(20.f, 420.f), SizeF(60.f, 60.f), LWCmode.ViewMode, this, Color.FromArgb(0xF1, 0xF1, 0xF1), Color.FromArgb(0xD8, 0xD8, 0xD8));
                     new LWCSelector(PointF(30.f, 500.f), SizeF(40.f, 40.f), LWCmode.ViewMode, this, Color.FromArgb(0x46, 0x46, 0xDA), Color.FromArgb(0x3F, 0x3F, 0xC4)) |]
    // Bottoni di navigazione
    let navbuts = [| new LWCNav(PointF(38.f, 20.f), SizeF(20.f, 20.f), LWCmode.ViewMode, this, Text = "U");
                     new LWCNav(PointF(18.f, 40.f), SizeF(20.f, 20.f), LWCmode.ViewMode, this, Text = "L");
                     new LWCNav(PointF(38.f, 60.f), SizeF(20.f, 20.f), LWCmode.ViewMode, this, Text = "D");
                     new LWCNav(PointF(58.f, 40.f), SizeF(20.f, 20.f), LWCmode.ViewMode, this, Text = "R");
                     new LWCNav(PointF(10.f, 10.f), SizeF(20.f, 20.f), LWCmode.ViewMode, this, Text = "Z");
                     new LWCNav(PointF(66.f, 10.f), SizeF(20.f, 20.f), LWCmode.ViewMode, this, Text = "X") |]

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
        buttons.[4].Text <- "Ball"
        buttons.[5].Text <- "ED"

        // Registra gli eventi per i tasti editor
        buttons.[0].MouseDown.Add(fun _ -> vectsheet.EditorState <- StateEditor.InsertCircle)
        buttons.[1].MouseDown.Add(fun _ -> vectsheet.EditorState <- StateEditor.InsertRectang)
        buttons.[2].MouseDown.Add(fun _ ->
            let gotFromTBox = textBox.Text
            let floatFromTB = System.Single.Parse(gotFromTBox)
            if floatFromTB > 0.f then vectsheet.SizePen <- floatFromTB
            else vectsheet.SizePen <- 1.f
            vectsheet.EditorState <- StateEditor.InsertLine
        )
        buttons.[3].MouseDown.Add(fun _ -> 
            let gotFromTBox = textBox.Text
            let floatFromTB = System.Single.Parse(gotFromTBox)
            if floatFromTB > 0.f then vectsheet.SizePen <- floatFromTB
            else vectsheet.SizePen <- 1.f
            vectsheet.EditorState <- StateEditor.InsertBezier
        )
        buttons.[4].MouseDown.Add(fun _ -> vectsheet.EditorState <- StateEditor.InsertBall)
        buttons.[5].MouseDown.Add(fun _ ->
            if vectsheet.EditorState = StateEditor.EditMode then
                vectsheet.ResetState
                vectsheet.EditorState <- StateEditor.DefaultState
            else
                vectsheet.EditorState <- StateEditor.EditMode
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

        navbuts.[int(NavBut.RLeft)].MouseDown.Add(fun _ -> vectsheet.PressedRLeft)
        navbuts.[int(NavBut.RLeft)].MouseUp.Add(fun _ -> vectsheet.UnPressedRLeft)

        navbuts.[int(NavBut.RRight)].MouseDown.Add(fun _ -> vectsheet.PressedRRight)
        navbuts.[int(NavBut.RRight)].MouseUp.Add(fun _ -> vectsheet.UnPressedRRight)

    override this.OnMouseDown e =
        base.OnMouseDown e

    override this.OnMouseUp e =
        base.OnMouseUp e
    
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
myForm.MinimumSize <- Size(1024, 620)
myForm.Controls.Add(vectForm)
myForm.Controls.Add(dockForm)
myForm.Show()