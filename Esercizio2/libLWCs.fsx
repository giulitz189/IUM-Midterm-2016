module libLWCs

(*
    Danilo Cianfrone - mat. 501292
    Esercizio 2 - libLWC potenziata

    La nuova libreria, libLWC, permette di specificare se il LWC da inserire in un LWCcontainer
    è da esprimere in coordinate mondo o in coordinate vista.

    Distinguiamo i vari casi:
        - LWCmode.ViewMode: coordinate vista, i LWC così specificati saranno sempre visibili nella
                            posizione in coordinate vista specificata (assumiamo, infatti, che nel
                            caso in cui un programmatore voglia specificare un comando in coordinate
                            vista, il programmatore desidera vedere tale comando sempre in quella
                            posizione ad ogni Invalidate() o repaint della form); in tal caso, il
                            controllo viene direttamente disegnato;

        - LWCmode.WorldMode: coordinate mondo, in questo caso, ogni volta che il controllo va disegnato
                             le coordinate vanno trasformate in coordinate vista tramite la matrice di
                             trasformazione W2V, che farà parte dell'LWCcontainer;
                             infatti, nella OnPaint() dell'LWCcontainer, andremo ad utilizzare le coordinate
                             vista, rese disponibili mediante il metodo ViewLoc(w2v) della classe LWC.
                             I controlli in coordinate mondo sono soggetti alle varie trasformazioni del
                             mondo, in quanto la necessità di utilizzare coordinate mondo può voler
                             significare inserire un controllo in un preciso punto mondo (per ragioni
                             a noi sconosciute).

    La libreria inoltre offre supporto al double buffering: nella classe LWCcontainer ci è possibile
    ricavare il contesto grafico del buffer (e inizializzarlo) grazie al metodo UseBuffer(), con la
    possibilità di sovrascriverlo in modo da offrire al programmatore cliente più flessibilità
    nella gestione delle proprie classi derivate da LWCcontainer (es. colore background diverso).

*)

open System.Windows.Forms
open System.Drawing

// Specifica in quale sistema viene costruito un LWC ---------------------------------------------- //
type LWCmode =
    | ViewMode  = 0
    | WorldMode = 1
// ------------------------------------------------------------------------------------------------ //

// ------------------------------------------------------------------------------------------------ //
type LWC(loc: PointF, siz: SizeF, mode: LWCmode, par: Control) =
    // Lo stato interno è mutable costruito a partire dal costruttore
    let mutable location = loc
    let mutable viewloc  = PointF()
    let mutable parent   = par
    let mutable size     = siz

    // Aggiorna le coordinate vista del controllo:
    //     - LWCmode.ViewMode:  rimangono le stesse;
    //     - LWCmode.WorldMode: vengono ricalcolate.
    let UpdateViewLoc(w2v: Drawing2D.Matrix) =
        if mode = LWCmode.ViewMode then viewloc <- loc
        else
            let t = [| location |]
            w2v.TransformPoints(t)
            viewloc <- t.[0]

    member this.Invalidate() =
        if parent <> null then parent.Invalidate()

    // Esegue l'update delle coordinate vista e le restituisce;
    // da usare in una classe derivata da LWCcontainer() [metodo pubblico].
    member this.ViewLoc(w2v: Drawing2D.Matrix) = UpdateViewLoc(w2v); viewloc

    member this.Parent   with get() = parent   and set(v) = parent   <- v
    member this.Size     with get() = size     and set(v) = size     <- v; this.Invalidate()
    member this.Location with get() = location and set(v) = location <- v; this.Invalidate()
    member this.Mode     with get() = mode
    // Dovrebbe essere "protected", ma F# non ci aiuta... pazienza!
    member this.PViewLoc with get() = viewloc

    abstract OnMouseDown : MouseEventArgs -> unit
    default this.OnMouseDown _ = ()

    abstract OnMouseMove : MouseEventArgs -> unit
    default this.OnMouseMove _ = ()

    abstract OnMouseUp : MouseEventArgs -> unit
    default this.OnMouseUp _ = ()

    abstract OnPaint : PaintEventArgs -> unit
    default this.OnPaint _ = ()

    abstract HitTest : PointF -> bool
    default this.HitTest p =
        (RectangleF(PointF(), size)).Contains(p)
// ------------------------------------------------------------------------------------------------ //

// ------------------------------------------------------------------------------------------------ //
type LWCcontainer() as this =
    inherit UserControl()

    // Matrici world-to-view e view-to-world
    let mutable v2w = new Drawing2D.Matrix()
    let mutable w2v = new Drawing2D.Matrix()

    // Il contenitore dei LWC è rappresentato tramite ResizeArray
    let lwControls = new ResizeArray<LWC>()

    let cloneMouseEvent (lwc: LWC) (e: MouseEventArgs) =
        // Punto in coordinate vista
        let p = lwc.ViewLoc(w2v)
        new MouseEventArgs(e.Button, e.Clicks, e.X, e.Y, e.Delta)

    let pickCorrelate (e: MouseEventArgs) (f: LWC -> MouseEventArgs -> unit) =
        // Guardia per indicare se il controllo selezionato dal mouse è stato trovato
        let mutable found = false
        // Itera sui controlli del container
        for i in { (lwControls.Count - 1) .. -1 .. 0 } do
            if not found then
                let lwc = lwControls.[i]
                let p   = lwc.ViewLoc(w2v)

                if lwc.HitTest(PointF(single(e.X) - p.X, single(e.Y) - p.Y)) then
                    found <- true
                    f lwc (cloneMouseEvent lwc e)

    // Indica il lightweight control catturato da gestire
    let mutable (captured: LWC option) = None

    // Implementazione double-buffering
    let mutable (buffer: Bitmap) = null
    let updateBuffer() =
        if buffer = null || buffer.Width <> this.Width || buffer.Height <> this.Height then
            if buffer <> null then buffer.Dispose()
            buffer <- new Bitmap(this.Width, this.Height)

    member this.V2W with get() = v2w and set(v) = v2w <- v
    member this.W2V with get() = w2v and set(v) = w2v <- v
    member this.LWControls = lwControls

    member this.Buffer = buffer
    member this.UpdateBuffer() = updateBuffer()

    // Da usare all'inizio della OnPaint della sottoclasse, per impostare il buffer
    abstract member UseBuffer : unit -> Graphics 
    default this.UseBuffer() =    
        updateBuffer()
        let gBCont  = Graphics.FromImage(buffer)
        let bgBrush = new SolidBrush(this.BackColor)
        gBCont.FillRectangle(bgBrush, 0, 0, buffer.Width, buffer.Height)
        gBCont

    override this.OnMouseDown e =
        pickCorrelate e (fun lwc ev -> captured <- Some(lwc); lwc.OnMouseDown(ev))
        base.OnMouseDown e

    override this.OnMouseUp e =
        pickCorrelate e (fun lwc ev -> lwc.OnMouseUp(ev))
        match captured with
        | Some lwc -> lwc.OnMouseUp(cloneMouseEvent lwc e); captured <- None
        | None     -> ()
        base.OnMouseUp e

    override this.OnMouseMove e =
        pickCorrelate e (fun lwc ev -> lwc.OnMouseMove(ev))
        match captured with
        | Some lwc -> lwc.OnMouseMove(cloneMouseEvent lwc e)
        | None     -> ()
        base.OnMouseMove e

    override this.OnPaintBackground e =
        ()

    override this.OnPaint e =
        lwControls |> Seq.iter (fun lwc ->
            let gBCont = Graphics.FromImage(buffer)
            let lwcPoint = lwc.ViewLoc(w2v)

            // Disegna il controllo solo se contenuto nella vista (form)
            if this.Bounds.Contains(Point(int(lwcPoint.X), int(lwcPoint.Y))) then
                let evt = new PaintEventArgs(gBCont, e.ClipRectangle)
                lwc.OnPaint evt
        )
        e.Graphics.DrawImage(buffer, 0, 0)
        base.OnPaint(e)

// ------------------------------------------------------------------------------------------------ //
