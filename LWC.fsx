// Michele Morisco 505252 
//Midterm IUM Settembre 2019
open System.Windows.Forms
open System.Drawing

let Rect2RectF (r:Rectangle) =
  RectangleF(single r.X, single r.Y, single r.Width, single r.Height)

let RectF2Rect (r:RectangleF) =
  Rectangle(int r.X, int r.Y, int r.Width, int r.Height)

let Point2PointF (p : Point) =
  PointF(single p.X, single p.Y)

type LWC() = 
   let mutable parent : Control = null 
   let mutable location = PointF() 
   let mutable size = SizeF()
   let mutable mondo = false
   let mutable w2v = new Drawing2D.Matrix()
   let mutable v2w = new Drawing2D.Matrix()
   let mutable graphicsp = new Drawing2D.GraphicsPath()
   let mousedownevt = new Event<MouseEventArgs>()
   let mousemoveevt = new Event<MouseEventArgs>()
   let mouseupevt = new Event<MouseEventArgs>()

   abstract OnMouseDown : MouseEventArgs -> unit
   default this.OnMouseDown _ = () 

   abstract OnMouseUp : MouseEventArgs -> unit
   default this.OnMouseUp _ = ()

   abstract OnMouseMove : MouseEventArgs -> unit
   default this.OnMouseMove _ = ()
 
   abstract OnPaint : PaintEventArgs -> unit 
   default this.OnPaint _ = ()

   abstract HitTest : PointF -> bool
   default this.HitTest p = // p è il puntatore del mouse
      (RectangleF(PointF(), size)).Contains(p) //p è in coordinate del contenitore non del contenuto

   member this.Invalidate() = 
      if (parent <> null) then parent.Invalidate()

   member this.Location
      with get() = location
      and set(v) = location <- v; this.Invalidate()

   member this.Size
      with get() = size
      and set(v) = size <- v; this.Invalidate()

   member this.Parent
      with get() = parent
      and set(v) = parent <- v //per assegnare o leggere il padre

   member this.Mondo
      with get() = mondo
      and set(v) = mondo <- v

   member this.W2V
      with get() = w2v
      and set(v) = w2v <- v

   member this.V2W
      with get() = v2w
      and set(v) = v2w <- v

   member this.Graphicsp
      with get() = graphicsp
      and set(v) = graphicsp <- v

type LWContainer() = 
   inherit UserControl()

   let controls = ResizeArray<LWC>() 

   let mutable w2v = new Drawing2D.Matrix()
   let mutable v2w = new Drawing2D.Matrix()


   let cloneMouseEvent (c:LWC) (e:MouseEventArgs) = 
      new MouseEventArgs(e.Button, e.Clicks, e.X - e.Location.Y, e.Y - e.Location.Y, e.Delta)

   let transformP (m:Drawing2D.Matrix) (p:Point) =
      let a = [| PointF(single p.X, single p.Y) |]
      m.TransformPoints(a)
      a.[0]

   let correlate (e:MouseEventArgs) (f: LWC->MouseEventArgs->unit) =
      let mutable found = false
      for i in { 0 .. 1 .. (controls.Count - 1)} do
         if not found then
            let c = controls.[i]
            if c.Mondo = false then
               if c.HitTest(PointF(single(e.X) - c.Location.X, single(e.Y) - c.Location.Y)) then
                  found <- true
                  let evt2 = new MouseEventArgs(e.Button, e.Clicks, e.X - int(c.Location.X), e.Y - int(e.Location.Y), e.Delta)
                  f c evt2
            else
               let t = transformP v2w e.Location
               if c.HitTest(PointF(t.X - c.Location.X, t.Y - c.Location.Y)) then
                  found <- true
                  let evt1 = new MouseEventArgs(e.Button, e.Clicks, e.X - int(c.Location.X), e.Y - int(e.Location.Y), e.Delta)
                  f c evt1

   let mutable captured : LWC option = None

   member this.LWControls = controls //restituisce la lista che altrimenti sarebbe privata

   member this.W2V 
      with get() = w2v
      and set(v) = w2v <- v

   member this.V2W
      with get() = v2w
      and set(v) = v2w <- v

   override this.OnMouseDown e = 
      correlate e (fun c ev -> captured <- Some(c); c.OnMouseDown(ev))
      base.OnMouseDown e

   override this.OnMouseUp e = 
      correlate e (fun c ev -> c.OnMouseUp(ev))
      match captured with
      | Some c -> c.OnMouseUp(cloneMouseEvent c e)
      | None -> ()
      base.OnMouseUp e

   override this.OnMouseMove e = 
      correlate e (fun c ev -> c.OnMouseMove(ev))
      match captured with
      | Some c -> c.OnMouseMove(cloneMouseEvent c e)
      | None -> ()
      base.OnMouseMove e


   override this.OnPaint e =
       let zbuffer = new ResizeArray<LWC>()
       let mutable j=0
       for i in (this.LWControls.Count - 1) .. -1 .. 0 do
         if this.LWControls.[i].Mondo = true then
            zbuffer.Add(this.LWControls.[i])
            zbuffer.[j].W2V <- this.LWControls.[i].W2V
            j <- j + 1
       for i in (zbuffer.Count - 1) .. -1 .. 0 do
            let s = e.Graphics.Save()
            e.Graphics.Transform <- zbuffer.[i].W2V
            let mutable r = e.Graphics.ClipBounds
            let evt = new PaintEventArgs(e.Graphics, new Rectangle(int(r.Left), int(r.Top), int(r.Width), int(r.Height)))
            zbuffer.[i].OnPaint evt
            e.Graphics.Restore(s)
       for i in (this.LWControls.Count - j - 1) .. -1 .. 0 do
       
            let s = e.Graphics.Save()
            e.Graphics.TranslateTransform(this.LWControls.[i].Location.X, this.LWControls.[i].Location.Y)
            e.Graphics.Clip <- new Region(RectangleF(0.f, 0.f, this.LWControls.[i].Size.Width, this.LWControls.[i].Size.Height))
            let mutable r = e.Graphics.ClipBounds
            let evt = new PaintEventArgs(e.Graphics, new Rectangle(int(r.Left), int(r.Top), int(r.Width), int(r.Height)))
            this.LWControls.[i].OnPaint evt
            e.Graphics.Restore(s)
      //)
       base.OnPaint(e)