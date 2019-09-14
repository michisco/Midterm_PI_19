// Michele Morisco 505252 
//Midterm IUM Settembre 2019
#load "LWC.fsx"

open System.Windows.Forms
open System.Drawing
open LWC

type Pad =
   | Up = 0
   | Down = 1
   | Left = 2
   | Right = 3

type RotateNav = 
   | RotateL = 0
   | RotateR = 1

type ZoomNav =
   | ZoomUp = 0
   | ZoomDown = 1

type MenuButton() =
    inherit LWC()

    let clickevt = new Event<System.EventArgs>()
    let downevt = new Event<System.EventArgs>()
    let upevt = new Event<System.EventArgs>()
    let moveevt = new Event<System.EventArgs>()

    let mutable text = ""
    let mutable selected = false
    let mutable color = new Color()
    let mutable myfont = new Font("Arial Black", 8.f)
    let mutable isVisible = true
    let mutable isDisable = false
    do color <- Color.FromArgb(211,47,47);

    member this.Click = clickevt.Publish
    member this.MouseDown = downevt.Publish
    member this.MouseUp = upevt.Publish
    member this.MouseMove = moveevt.Publish

    member this.Text
        with get() = text
        and set(v) = text <- v; this.Invalidate()

    member this.Color
        with get() = color
        and set(v) = color <- v; this.Invalidate()

    member this.ColorTransparent
        with get() = Color.FromArgb(100, color)

    member this.Selected
        with get() = selected
        and set(v) = (selected <- v; this.SelectedColor() )

    member this.SelectedColor () =
        if this.Selected then this.Color <- Color.FromArgb(171,0,13)
        else this.Color <- Color.FromArgb(211,47,47)
        this.Invalidate()

    member this.Font
        with get() = myfont
        and set(v) = myfont <- v
    
    member this.Visible 
        with get() = isVisible
        and set(v) = isVisible <- v

    member this.Disable
        with get() = isDisable
        and set(v) = isDisable <- v
   
    override this.OnMouseUp e = upevt.Trigger(e); clickevt.Trigger(new System.EventArgs())
    override this.OnMouseMove e = moveevt.Trigger(e)
    override this.OnMouseDown e = downevt.Trigger(e)
   
type SquareButton() as this =
  inherit MenuButton()
  do this.Size <- SizeF(64.f, 64.f)

  override this.OnPaint e =
    if this.Visible then
        let g = e.Graphics
        let sz = g.MeasureString(this.Text, this.Font)        
        if this.Disable then
            use bcolor = new SolidBrush(this.ColorTransparent)        
            g.FillRectangle(bcolor, 0.f , 0.f, this.Size.Width, this.Size.Height)
        else 
            use bcolor = new SolidBrush(this.Color)
            g.FillRectangle(bcolor, 0.f , 0.f, this.Size.Width, this.Size.Height)
        g.DrawString(this.Text, this.Font, Brushes.White, PointF((this.Size.Width - sz.Width) / 2.f, (this.Size.Height - sz.Height) / 2.f))

type CircleButton() as this =
  inherit MenuButton()
  
  do this.Size <- SizeF(50.f, 50.f)
  
  override this.OnPaint e =
    if this.Visible then
        let g = e.Graphics
        let sz = g.MeasureString(this.Text, this.Font)
        if this.Disable then
            use bcolor = new SolidBrush(this.ColorTransparent)      
            g.FillEllipse(bcolor, 0.f , 0.f, this.Size.Width, this.Size.Height)
        else 
            use bcolor = new SolidBrush(this.Color)
            g.FillEllipse(bcolor, 0.f , 0.f, this.Size.Width, this.Size.Height)       
        g.DrawString(this.Text, this.Font, Brushes.White, PointF((this.Size.Width - sz.Width) / 2.f, (this.Size.Height - sz.Height) / 2.f))