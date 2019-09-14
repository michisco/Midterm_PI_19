// Michele Morisco 505252 
//Midterm IUM Settembre 2019
#load "LWC.fsx"
#load "MenuButtons.fsx"

open System.Drawing
open System.Windows.Forms
open System.Drawing.Drawing2D
open System.Collections.Generic
open LWC
open MenuButtons

type NavBut=
   | Track = 3
   | Select = 2
   | Delete = 1
   | Animate = 0

let f = new Form(Text="Track Midterm",TopMost=true,Size=Size(980,600))
f.Show()

type PreviewT(p1: Point, p2: Point) =
    inherit LWC()

    let mutable selected = false
   
    member this.Selected
        with get() = selected
        and set(v) = selected <- v
        
    override this.OnMouseDown e = 
        base.OnMouseDown(e)

     override this.OnMouseMove e = 
        base.OnMouseMove(e)

    override this.OnPaint(e) =
        let g = e.Graphics   
        use b = new SolidBrush(Color.Gray)
        //g.FillEllipse(b, box)
        use p = new Pen(b, float32 6.f)
        p.StartCap  <- LineCap.Round 
        p.EndCap <- LineCap.Round
        g.DrawLine(p, p1, p2)
        base.OnPaint(e)

type Car(r: Rectangle) =
    inherit LWC()

    let mutable speed = 1.f
    let mutable color = new Color()
    let mutable box = r 
    let rands = System.Random()
    do 
       color <- Color.FromArgb(rands.Next(254), rands.Next(40, 254), rands.Next(40, 254));
       speed <- float32 (rands.Next(40));

    member this.Location
       with get() = PointF(float32 box.X, float32 box.Y)
       and set(v: PointF) = 
            box <- Rectangle (int v.X, int v.Y, box.Width, box.Height)            
    member this.Color
       with get() = color
    member this.Speed
       with get() = speed
    member this.Vehicle 
       with get() = box
    
    override this.OnPaint(e) =
        let g = e.Graphics      
        use b = new SolidBrush(color)
        g.FillEllipse(b, box)
        base.OnPaint(e) 

type Track(start: Point) = 
    inherit LWC()

    let mutable points = ResizeArray<Point>()
   // let mutable trackPoints = [||]
    let mutable selected = false
    let mutable drawingMode = false
    let mutable polygonBox = new GraphicsPath()
    let mutable box = new RectangleF()
    let mutable car = new Car(Rectangle(0, 0, 30, 30))
    let mutable nextPosition = 1
    let mutable animateState = false
    let mutable pauseState = false
    
    member this.GetRect() =
        box <- polygonBox.GetBounds()
    member this.Contains (x, y) =
           box.Contains(x, y)
    member this.X
           with get () = int box.X
           and set(v) = 
               box <- Rect2RectF(Rectangle(v, int box.Y, int box.Width, int box.Height))
    member this.Y
           with get () = int box.Y
           and set(v) = 
               box <- Rect2RectF(Rectangle(int box.X, v, int box.Width, int box.Height))
    member this.Location
           with get() = PointF(box.X, box.Y)
           and set(v:PointF) = 
               box <- Rect2RectF(Rectangle (int v.X, int v.Y, int box.Width, int box.Height))
               for i in points.Count-1..-1..0 do
                   points.[i] <- Point(points.[i].X - int box.X, points.[i].Y - int box.Y)
    member this.Selected
        with get() = selected
        and set(v) = selected <- v
    member this.GoCar 
        with get() = animateState
        and set(v) = animateState <- v
    member this.PauseCar
        with get() = pauseState
        and set(v) = pauseState <- v
    member this.AddPoint(v) = points.Add(v)
    member this.GetTrack
        with get() = points
    member this.DeleteTrack() = points.Clear()
    member this.DrawingState 
        with get() = drawingMode
        and set(v) = drawingMode <- v 
    member this.CreatePath() = 
       polygonBox.AddPolygon(points.ToArray())
       car.Location <- Point2PointF(points.[0])
    member this.GetPoint(v) = points.[v]
    member this.SetPoint(v, newP: Point) = points.[v] <- newP
    member this.UpdateCarLocation = 
      if not(this.PauseCar) then
        let tx = float32(this.GetPoint(nextPosition).X) -  car.Location.X;
        let ty = float32(this.GetPoint(nextPosition).Y) -  car.Location.Y;
        let sum = tx*tx + ty*ty
        let length = sqrt(float32 sum);
        
        if length > car.Speed then
            // move towards the goal
            let newPosX = float32 car.Location.X + car.Speed*tx/length
            let newPosY = float32 car.Location.Y + car.Speed*ty/length
            let newPoint = PointF(newPosX, newPosY)
            car.Location <- newPoint;
        else
            let newLocation = Point2PointF(this.GetPoint(nextPosition))
            
            car.Location <- newLocation
            if nextPosition = (points.Count - 1) then 
                nextPosition <- 1
            else
                nextPosition <- nextPosition + 1
      else 
        nextPosition <- 1

    override this.OnPaint(e) =
        let g = e.Graphics      
        use b = new SolidBrush(Color.Gray)
        use p = new Pen(b, 6.f)
        p.StartCap  <- LineCap.Round 
        p.LineJoin <- LineJoin.Round
        p.EndCap <- LineCap.Round

        if this.DrawingState then 
            if points.Count > 1 then
                g.DrawLines(p, points.ToArray())
                //trackPoints <- polygon.PathPoints
        if this.Selected then
            use pS = new Pen(Color.Gray)
            pS.DashStyle <- Drawing2D.DashStyle.Dash //tratteggiatura
            g.DrawRectangle(pS, RectF2Rect(box))

            //this.GetRect()
            //g.DrawRectangle(pS, RectF2Rect(box))
        if this.GoCar then
            use c = new SolidBrush(car.Color)
            g.FillEllipse(c, car.Vehicle)
            base.OnPaint(e)  
            
        base.OnPaint(e)
   
type MainTrack() as this =
    inherit LWContainer()
    do 
        this.SetStyle(ControlStyles.DoubleBuffer,true)
        this.SetStyle(ControlStyles.AllPaintingInWmPaint,true)
        this.SetStyle(ControlStyles.UserPaint,true)

    let mutable startDrawing = false
    let mutable prevPoint = new Point()
    let mutable tracks = ResizeArray<Track>()
    let mutable paintP = ResizeArray<PreviewT>()
    let mutable dragTrack = None //drag 'n drop tracciato
    let mutable onceClick = false
    let mutable tracksSel = ResizeArray<Track>() //array di tracciati selezionati 
    let mutable carsMoving = new ResizeArray<Track>() //array dei tracciati con le auto

    let mutable drawMode = false
    //let mutable track = new Track()
    let mutable w2v = new Drawing2D.Matrix()
    let mutable v2w = new Drawing2D.Matrix()
 
    let buttonsNav = [|      
        new SquareButton(Text="Animate",Location=PointF(single(f.ClientSize.Width)-64.f,single(f.ClientSize.Height)-64.f), Color=Color.FromArgb(255,102,89));
        new SquareButton(Text="Delete",Location=PointF(single(f.ClientSize.Width)-128.f,single(f.ClientSize.Height)-64.f), Color=Color.FromArgb(255,102,89));      
        new SquareButton(Text="Select",Location=PointF(single(f.ClientSize.Width)-192.f,single(f.ClientSize.Height)-64.f), Color=Color.FromArgb(255,102,89));
        new SquareButton(Text="Track",Location=PointF(single(f.ClientSize.Width)-256.f, single(f.ClientSize.Height)-64.f), Color=Color.FromArgb(255,102,89));  
    |]
    let buttonsCircl = [|
        new CircleButton(Text="Up",Location=PointF(55.f,20.f));
        new CircleButton(Text="Down",Location=PointF(55.f,100.f));
        new CircleButton(Text="Left",Location=PointF(5.f,60.f));
        new CircleButton(Text="Right",Location=PointF(105.f,60.f));

    |]
    let buttonsRot = [|
        new SquareButton(Text="Rotate-L",Location=PointF(0.f,single(f.ClientSize.Height)-64.f));
        new SquareButton(Text="Rotate-R",Location=PointF(64.f,single(f.ClientSize.Height)-64.f));
    |]

    let buttonsZoom = [|
        new SquareButton(Text="Zoom In",Location=PointF(128.f,single(f.ClientSize.Height)-64.f));
        new SquareButton(Text="Zoom Out",Location=PointF(192.f,single(f.ClientSize.Height)-64.f)); 
    |]

    do buttonsNav |> Seq.iter (fun b ->
        b.Parent <- this;
        this.LWControls.Add(b)
    )

    do buttonsRot |> Seq.iter (fun b ->
        b.Parent <- this;
        this.LWControls.Add(b)
    )

    do buttonsZoom |> Seq.iter (fun b ->
        b.Parent <- this;
        this.LWControls.Add(b)
    )

    do buttonsCircl |> Seq.iter (fun b ->
        b.Parent <- this;
        this.LWControls.Add(b)
    )

    let updateCar() =
        carsMoving |> Seq.iter (fun x -> 
            x.UpdateCarLocation                  
        )
        this.Invalidate()

    let timerCar = new Timer(Interval=20)
    do timerCar.Tick.Add(fun _  -> updateCar() ; this.Invalidate())

//---------------------OPERAZIONI BOTTONI SPECIALI------------------//
    do
          buttonsNav.[int(NavBut.Track)].MouseDown.Add(fun _ ->  
              buttonsNav.[int(NavBut.Track)].Selected <- true
              buttonsNav.[int(NavBut.Track)].Color <- Color.FromArgb(171,0,13);

              buttonsNav.[int(NavBut.Select)].Selected <- false
              buttonsNav.[int(NavBut.Select)].Color <- Color.FromArgb(255,102,89);
              tracks |> Seq.iter (fun b -> b.Selected <- false)
              tracksSel.Clear()
          )

          buttonsNav.[int(NavBut.Select)].MouseDown.Add(fun _ -> 
              if not(buttonsNav.[int(NavBut.Select)].Disable) then
                buttonsNav.[int(NavBut.Select)].Selected <- true
                buttonsNav.[int(NavBut.Select)].Color <- Color.FromArgb(171,0,13);

                buttonsNav.[int(NavBut.Track)].Selected <- false
                buttonsNav.[int(NavBut.Track)].Color <- Color.FromArgb(255,102,89);
                startDrawing <- false
          )

          buttonsNav.[int(NavBut.Delete)].MouseDown.Add(fun _ ->  
            if not(buttonsNav.[int(NavBut.Delete)].Disable) then
              buttonsNav.[int(NavBut.Delete)].Selected <- true
              buttonsNav.[int(NavBut.Delete)].Color <- Color.FromArgb(171,0,13);

              buttonsNav.[int(NavBut.Track)].Selected <- false
              buttonsNav.[int(NavBut.Track)].Color <- Color.FromArgb(255,102,89);
              buttonsNav.[int(NavBut.Select)].Selected <- false
              buttonsNav.[int(NavBut.Select)].Color <- Color.FromArgb(255,102,89);
              startDrawing <- false

              tracksSel |> Seq.iter (fun b ->
                  tracks.Remove(b) |> ignore
                  this.LWControls.Remove(b) |> ignore
                  this.Invalidate())
              tracksSel.Clear()
          )
          buttonsNav.[int(NavBut.Delete)].MouseUp.Add(fun _ ->
              buttonsNav.[int(NavBut.Delete)].Selected <- false
              buttonsNav.[int(NavBut.Delete)].Color <- Color.FromArgb(255,102,89);
          )

          buttonsNav.[int(NavBut.Animate)].MouseDown.Add(fun _ -> 
            if not(buttonsNav.[int(NavBut.Animate)].Disable) then
              buttonsNav.[int(NavBut.Animate)].Selected <- true
              buttonsNav.[int(NavBut.Animate)].Color <- Color.FromArgb(171,0,13);

              if tracksSel.Count > 0 then
                for i in tracksSel.Count-1..-1..0 do
                    if not(carsMoving.Contains(tracksSel.[i])) then
                        tracksSel.[i].GoCar <- true
                        carsMoving.Add(tracksSel.[i])                     
                timerCar.Start()

              buttonsNav.[int(NavBut.Track)].Selected <- false
              buttonsNav.[int(NavBut.Track)].Color <- Color.FromArgb(255,102,89);
              buttonsNav.[int(NavBut.Select)].Selected <- false
              buttonsNav.[int(NavBut.Select)].Color <- Color.FromArgb(255,102,89);
              startDrawing <- false
              buttonsNav.[int(NavBut.Select)].Selected <- false
              buttonsNav.[int(NavBut.Select)].Color <- Color.FromArgb(255,102,89);
              tracks |> Seq.iter (fun b -> b.Selected <- false)
              tracksSel.Clear()
          )
          buttonsNav.[int(NavBut.Animate)].MouseUp.Add(fun _ ->
              buttonsNav.[int(NavBut.Animate)].Selected <- false
              buttonsNav.[int(NavBut.Animate)].Color <- Color.FromArgb(255,102,89);
          )

    //---------------------OPERAZIONI DI TRASFORMAZIONE------------------//
    let translateW (tx, ty) =
          w2v.Translate(tx, ty)
          v2w.Translate(-tx, -ty, Drawing2D.MatrixOrder.Append)
    let rotateW a =
          w2v.Rotate a
          v2w.Rotate(-a, Drawing2D.MatrixOrder.Append)

    let rotateAtW p a =
          w2v.RotateAt(a, p)
          v2w.RotateAt(-a, p, Drawing2D.MatrixOrder.Append)

    let scaleW (sx, sy) =
          w2v.Scale(sx, sy)
          v2w.Scale(1.f/sx, 1.f/sy, Drawing2D.MatrixOrder.Append)
    
    let transformP (m:Drawing2D.Matrix) (p:Point) =
          let a = [| PointF(single p.X, single p.Y) |]
          m.TransformPoints(a)
          a.[0]

    let translate (x, y) =
          let t = [| PointF(0.f, 0.f); PointF(x, y) |]
          v2w.TransformPoints(t)
          translateW(t.[1].X - t.[0].X, t.[1].Y - t.[0].Y)

    //spostare un singolo oggetto
    let translateObj (x, y, obj: Track) =
          obj |> (fun b ->
              let t = [| PointF(float32 b.X, float32 b.Y); PointF(x, y) |]
              b.V2W.TransformPoints(t)
              b.W2V.Translate(t.[1].X - t.[0].X, t.[1].Y - t.[0].Y)
              b.V2W.Translate(-(t.[1].X - t.[0].X), -(t.[1].Y - t.[0].Y), Drawing2D.MatrixOrder.Append)
          )

    let translateAll (x, y) =
          tracks |> Seq.iter(fun b ->
              let t = [| PointF(0.f, 0.f); PointF(x, y) |]
              b.V2W.TransformPoints(t)
              b.W2V.Translate(t.[1].X - t.[0].X, t.[1].Y - t.[0].Y)
              b.V2W.Translate(-(t.[1].X - t.[0].X), -(t.[1].Y - t.[0].Y), Drawing2D.MatrixOrder.Append)
          )

    let rotateAll x =
          tracks |> Seq.iter(fun b ->
            let p = transformP b.V2W (Point(this.Width / 2, this.Height / 2))
            b.W2V.RotateAt(x, p)
            b.V2W.RotateAt(-x, p, Drawing2D.MatrixOrder.Append)  
          )

    let zoomAll x =
          tracks |> Seq.iter(fun b ->
            let p = transformP b.V2W (Point(this.Width / 2, this.Height / 2))
            b.W2V.Scale(x, x)
            b.V2W.Scale(1.f/x, 1.f/x, Drawing2D.MatrixOrder.Append)
            let p1 = transformP b.V2W (Point(this.Width / 2, this.Height / 2))
            b.W2V.Translate(p1.X - p.X, p1.Y - p.Y)
            b.V2W.Translate(-(p1.X - p.X), -(p1.Y - p.Y), Drawing2D.MatrixOrder.Append)
          )

    let RotateBy dir =
          match dir with
          | RotateNav.RotateL -> -10.f
          | RotateNav.RotateR -> 10.f
          | _ -> 0.f

    let scrollBy dir =
           match dir with
           | Pad.Up -> (0.f,-10.f)
           | Pad.Down -> (0.f,10.f)
           | Pad.Left -> (-10.f,0.f)
           | Pad.Right -> (10.f,0.f)
           | _ -> (0.f,0.f)

    let ZoomBy dir =
           match dir with
           | ZoomNav.ZoomUp -> 1.1f
           | ZoomNav.ZoomDown -> (1.f / 1.1f)
           | _ -> 0.f

    let rotate x =
          let p = transformP v2w (Point(this.Width / 2, this.Height / 2))
          rotateAtW p x
    
    let TrackHitTest (mousew:Point) =
        // selezionando un tracciato
        let t = tracks |> Seq.tryFindBack (fun box ->
            let p = transformP box.V2W mousew   
            box.Contains(p.X, p.Y))
        match t with
        | Some b -> 
            b.Selected <- true;
            let p = transformP b.V2W mousew  
            let dx, dy = p.X - float32 b.X, p.Y - float32 b.Y
            let pp = ResizeArray<Point>()
            for i in 0..b.GetTrack.Count - 1 do
                let dx1, dy1 = p.X - float32 (b.GetPoint(i).X), p.Y - float32 (b.GetPoint(i).Y)
                pp.Add(Point(int dx1, int dy1))
            dragTrack <- Some(b, dx, dy, pp)
        | _ -> () 
        this.Invalidate()
        
 //-------------------PULSANTI-------------//
    let handleCommand (k:Keys) =
        match k with
        | Keys.W -> 
          translateAll(0.f,-10.f)
          this.Invalidate()
        | Keys.D -> 
          translateAll(10.f,0.f)
          this.Invalidate()
        | Keys.A -> 
          translateAll(-10.f,0.f)
          this.Invalidate()
        | Keys.S -> 
          translateAll(0.f,10.f)
          this.Invalidate()
        | Keys.Q -> 
          rotateAll -10.f
          this.Invalidate()
        | Keys.E ->
          rotateAll 10.f
          this.Invalidate()
        | Keys.Z ->
          zoomAll 1.1f
          this.Invalidate()
        | Keys.X ->
          zoomAll (1.f / 1.1f)
          this.Invalidate()
        | _ -> ()
 //-------------------FINE PULSANTI--------------//
          
//----------------TIMER------------------//
    let scrollTimer = new Timer(Interval = 40)
    let rotateTimer = new Timer(Interval = 40)
    let zoomTimer = new Timer(Interval = 40)
    let mutable scrollDir = Pad.Up
    let mutable rotateDir = RotateNav.RotateL
    let mutable zoomDir = ZoomNav.ZoomUp    

    do scrollTimer.Tick.Add(fun _ ->
         scrollBy scrollDir |> translateAll
         this.Invalidate()
    )
    do rotateTimer.Tick.Add(fun _ ->
         RotateBy rotateDir |> rotateAll
         this.Invalidate()
    )
    do zoomTimer.Tick.Add(fun _ ->
         ZoomBy zoomDir |> zoomAll
         this.Invalidate()
    )
 //---------FINE TIMER----------//

 //-----------PULSANTI PER LE TRASFORMAZIONI----------//
    do
        buttonsCircl.[int(Pad.Up)].MouseDown.Add(fun _ -> 
          if not(buttonsCircl.[int(Pad.Up)].Disable) then
            scrollDir <- Pad.Up
            this.Invalidate()
            buttonsCircl.[int(Pad.Up)].Selected <- true
            // buttonsCircl.[int(Pad.Up)].Color <- Color.FromArgb(171,0,13);

            buttonsNav.[int(NavBut.Track)].Selected <- false
            buttonsNav.[int(NavBut.Track)].Color <- Color.FromArgb(255,102,89);
            startDrawing <- false
            buttonsNav.[int(NavBut.Select)].Selected <- false
            buttonsNav.[int(NavBut.Select)].Color <- Color.FromArgb(255,102,89);
            tracks |> Seq.iter (fun b -> b.Selected <- false)
            tracksSel.Clear()
        )
        buttonsCircl.[int(Pad.Up)].MouseUp.Add(fun _ ->
            buttonsCircl.[int(Pad.Up)].Selected <- false
            // buttonsCircl.[int(Pad.Up)].Color <- Color.FromArgb(211,47,47);
        )
        //UP
        buttonsCircl.[int(Pad.Down)].MouseDown.Add(fun _ -> 
          if not(buttonsCircl.[int(Pad.Down)].Disable) then
            scrollDir <- Pad.Down
            buttonsCircl.[int(Pad.Down)].Selected <- true
            // buttonsCircl.[int(Pad.Down)].Color <- Color.FromArgb(171,0,13);

            buttonsNav.[int(NavBut.Track)].Selected <- false
            buttonsNav.[int(NavBut.Track)].Color <- Color.FromArgb(255,102,89);
            startDrawing <- false
            buttonsNav.[int(NavBut.Select)].Selected <- false
            buttonsNav.[int(NavBut.Select)].Color <- Color.FromArgb(255,102,89);
            tracks |> Seq.iter (fun b -> b.Selected <- false)
            tracksSel.Clear()
        )
        buttonsCircl.[int(Pad.Down)].MouseUp.Add(fun _ ->
            buttonsCircl.[int(Pad.Down)].Selected <- false
            // buttonsCircl.[int(Pad.Down)].Color <- Color.FromArgb(211,47,47);
        )
        //DOWN
        buttonsCircl.[int(Pad.Left)].MouseDown.Add(fun _ -> 
          if not(buttonsCircl.[int(Pad.Left)].Disable) then
            scrollDir <- Pad.Left
            buttonsCircl.[int(Pad.Left)].Selected <- true
            // buttonsCircl.[int(Pad.Left)].Color <- Color.FromArgb(171,0,13);

            buttonsNav.[int(NavBut.Track)].Selected <- false
            buttonsNav.[int(NavBut.Track)].Color <- Color.FromArgb(255,102,89);
            startDrawing <- false
            buttonsNav.[int(NavBut.Select)].Selected <- false
            buttonsNav.[int(NavBut.Select)].Color <- Color.FromArgb(255,102,89);
            tracks |> Seq.iter (fun b -> b.Selected <- false)
            tracksSel.Clear()
        )
        buttonsCircl.[int(Pad.Left)].MouseUp.Add(fun _ ->
            buttonsCircl.[int(Pad.Left)].Selected <- false
            // buttonsCircl.[int(Pad.Left)].Color <- Color.FromArgb(211,47,47);
        )
        //LEFT
        buttonsCircl.[int(Pad.Right)].MouseDown.Add(fun _ -> 
          if not(buttonsCircl.[int(Pad.Right)].Disable) then
            scrollDir <- Pad.Right
            buttonsCircl.[int(Pad.Right)].Selected <- true
            //buttonsCircl.[int(Pad.Right)].Color <- Color.FromArgb(171,0,13);

            buttonsNav.[int(NavBut.Track)].Selected <- false
            buttonsNav.[int(NavBut.Track)].Color <- Color.FromArgb(255,102,89);
            startDrawing <- false
            buttonsNav.[int(NavBut.Select)].Selected <- false
            buttonsNav.[int(NavBut.Select)].Color <- Color.FromArgb(255,102,89);
            tracks |> Seq.iter (fun b -> b.Selected <- false)
            tracksSel.Clear()
        )
        buttonsCircl.[int(Pad.Right)].MouseUp.Add(fun _ ->
            buttonsCircl.[int(Pad.Right)].Selected <- false
            //buttonsCircl.[int(Pad.Right)].Color <- Color.FromArgb(211,47,47);
        )
        //RIGHT
        buttonsRot.[int(RotateNav.RotateL)].MouseDown.Add(fun _ -> 
          if not(buttonsRot.[int(RotateNav.RotateL)].Disable) then
            rotateDir <- RotateNav.RotateL
            buttonsRot.[int(RotateNav.RotateL)].Selected <- true
            // buttonsRot.[int(RotateNav.RotateL)].Color <- Color.FromArgb(171,0,13);

            buttonsNav.[int(NavBut.Track)].Selected <- false
            buttonsNav.[int(NavBut.Track)].Color <- Color.FromArgb(255,102,89);
            startDrawing <- false
            buttonsNav.[int(NavBut.Select)].Selected <- false
            buttonsNav.[int(NavBut.Select)].Color <- Color.FromArgb(255,102,89);
            tracks |> Seq.iter (fun b -> b.Selected <- false)
            tracksSel.Clear()
        )
        buttonsRot.[int(RotateNav.RotateL)].MouseUp.Add(fun _ ->
            buttonsRot.[int(RotateNav.RotateL)].Selected <- false
            // buttonsRot.[int(RotateNav.RotateL)].Color <- Color.FromArgb(211,47,47);
        )
        //ROTATE LEFT
        buttonsRot.[int(RotateNav.RotateR)].MouseDown.Add(fun _ -> 
          if not(buttonsRot.[int(RotateNav.RotateR)].Disable) then
            rotateDir <- RotateNav.RotateR
            buttonsRot.[int(RotateNav.RotateR)].Selected <- true
            //buttonsRot.[int(RotateNav.RotateR)].Color <- Color.FromArgb(171,0,13);

            buttonsNav.[int(NavBut.Track)].Selected <- false
            buttonsNav.[int(NavBut.Track)].Color <- Color.FromArgb(255,102,89);
            startDrawing <- false
            buttonsNav.[int(NavBut.Select)].Selected <- false
            buttonsNav.[int(NavBut.Select)].Color <- Color.FromArgb(255,102,89);
            tracks |> Seq.iter (fun b -> b.Selected <- false)
            tracksSel.Clear()
        )
        buttonsRot.[int(RotateNav.RotateR)].MouseUp.Add(fun _ ->
            buttonsRot.[int(RotateNav.RotateR)].Selected <- false
            // buttonsRot.[int(RotateNav.RotateR)].Color <- Color.FromArgb(211,47,47);
        )
        //ROTATE RIGHT
        buttonsZoom.[int(ZoomNav.ZoomUp)].MouseDown.Add(fun _ -> 
          if not(buttonsZoom.[int(ZoomNav.ZoomUp)].Disable) then 
            zoomDir <- ZoomNav.ZoomUp   
            buttonsZoom.[int(ZoomNav.ZoomUp)].Selected <- true
            // buttonsZoom.[int(ZoomNav.ZoomUp)].Color <- Color.FromArgb(171,0,13);

            buttonsNav.[int(NavBut.Track)].Selected <- false
            buttonsNav.[int(NavBut.Track)].Color <- Color.FromArgb(255,102,89);
            startDrawing <- false
            buttonsNav.[int(NavBut.Select)].Selected <- false
            buttonsNav.[int(NavBut.Select)].Color <- Color.FromArgb(255,102,89);
            tracks |> Seq.iter (fun b -> b.Selected <- false)
            tracksSel.Clear()
        )
        buttonsZoom.[int(ZoomNav.ZoomUp)].MouseUp.Add(fun _ ->
            buttonsZoom.[int(ZoomNav.ZoomUp)].Selected <- false
            // buttonsZoom.[int(ZoomNav.ZoomUp)].Color <- Color.FromArgb(211,47,47);
        )
        //ZOOM IN
        buttonsZoom.[int(ZoomNav.ZoomDown)].MouseDown.Add(fun _ -> 
          if not(buttonsZoom.[int(ZoomNav.ZoomDown)].Disable) then
            zoomDir <- ZoomNav.ZoomDown
            buttonsZoom.[int(ZoomNav.ZoomDown)].Selected <- true
            //buttonsZoom.[int(ZoomNav.ZoomDown)].Color <- Color.FromArgb(171,0,13);

            buttonsNav.[int(NavBut.Track)].Selected <- false
            buttonsNav.[int(NavBut.Track)].Color <- Color.FromArgb(255,102,89);
            startDrawing <- false
            buttonsNav.[int(NavBut.Select)].Selected <- false
            buttonsNav.[int(NavBut.Select)].Color <- Color.FromArgb(255,102,89);
            tracks |> Seq.iter (fun b -> b.Selected <- false)
            tracksSel.Clear()
        )
        buttonsZoom.[int(ZoomNav.ZoomDown)].MouseUp.Add(fun _ ->
            buttonsZoom.[int(ZoomNav.ZoomDown)].Selected <- false
            // buttonsZoom.[int(ZoomNav.ZoomDown)].Color <- Color.FromArgb(211,47,47);
        )
        //ZOOM OUT
        //per il funzionamento dei tasti
        for v in [ Pad.Up; Pad.Down; Pad.Left; Pad.Right; ] do
          let idx = int(v)
          buttonsCircl.[idx].MouseDown.Add(fun _ -> scrollTimer.Start())
          buttonsCircl.[idx].MouseUp.Add(fun _ -> scrollTimer.Stop())
        for v in [ RotateNav.RotateL; RotateNav.RotateR ] do
          let idx = int(v)
          buttonsRot.[idx].MouseDown.Add(fun _ -> rotateTimer.Start())
          buttonsRot.[idx].MouseUp.Add(fun _ -> rotateTimer.Stop())
        for v in [ ZoomNav.ZoomUp; ZoomNav.ZoomDown ] do
          let idx = int(v)
          buttonsZoom.[idx].MouseDown.Add(fun _ -> zoomTimer.Start())
          buttonsZoom.[idx].MouseUp.Add(fun _ -> zoomTimer.Stop())
 //-----------FINE PULSANTI PER LE TRASFORMAZIONI--------//

    override this.OnMouseDown e =   
        if buttonsNav.[int(NavBut.Track)].Selected && not(onceClick) then
            do
                startDrawing <- true
                //disabilito tutti i bottoni
                buttonsZoom.[int(ZoomNav.ZoomUp)].Disable <- true
                buttonsZoom.[int(ZoomNav.ZoomDown)].Disable <- true
                buttonsCircl.[int(Pad.Up)].Disable <- true
                buttonsCircl.[int(Pad.Down)].Disable <- true
                buttonsCircl.[int(Pad.Left)].Disable <- true
                buttonsCircl.[int(Pad.Right)].Disable <- true
                buttonsRot.[int(RotateNav.RotateR)].Disable <- true
                buttonsRot.[int(RotateNav.RotateL)].Disable <- true
                buttonsNav.[int(NavBut.Track)].Disable <- true
                buttonsNav.[int(NavBut.Select)].Disable <- true
                buttonsNav.[int(NavBut.Delete)].Disable <- true
                buttonsNav.[int(NavBut.Animate)].Disable <- true

                prevPoint <- Point(e.X, e.Y) 
                let t = new  Track(prevPoint, Mondo = true, Parent = this)
                tracks.Add(t)
                onceClick <- true
        elif buttonsNav.[int(NavBut.Select)].Selected then
            // selezionando un tracciato
            let t = tracks |> Seq.tryFindBack (fun box ->
                let p = transformP box.V2W e.Location    
                box.Contains(p.X, p.Y))
            match t with
            | Some b -> 
                b.Selected <- true; 
                if not(tracksSel.Contains(b)) then
                    tracksSel.Add(b)
            | _ -> () 
        else
            TrackHitTest e.Location
            this.Invalidate()
        base.OnMouseDown(e)
        
    override this.OnMouseMove e = 
        if startDrawing then
            do
                drawMode <- true
                let p = new PreviewT(prevPoint, e.Location, Mondo = true, Parent = this)
                paintP.Add(p)
                tracks.[tracks.Count - 1].AddPoint(e.Location)
                prevPoint <- Point(e.X, e.Y)
                this.LWControls.Add(p)
        elif buttonsNav.[int(NavBut.Select)].Selected then
            ()
        else
            let newpoint = [|Point(e.X, e.Y)|]
            match dragTrack with
            | Some(box, dx, dy, pp) ->
                let p = transformP box.V2W newpoint.[0]   
                box.Location <- PointF(p.X - dx, p.Y - dy)
                box.PauseCar <- true
                let pp2 = ResizeArray<Point>()
                for i in 0..box.GetTrack.Count - 1 do
                    let dx1, dy1 = p.X - float32 (pp.[i].X), p.Y - float32 (pp.[i].Y)
                    pp2.Add(Point(int dx1, int dy1))
                box.DeleteTrack()
                for i in 0..pp2.Count - 1 do
                    box.AddPoint(pp2.[i])
                box.CreatePath()
            | _ -> ()
        this.Invalidate()
        base.OnMouseMove(e) 
      
    override this.OnMouseUp e =
        if drawMode then
            startDrawing <- false
            //disabilito tutti i bottoni
            buttonsZoom.[int(ZoomNav.ZoomUp)].Disable <- false
            buttonsZoom.[int(ZoomNav.ZoomDown)].Disable <- false
            buttonsCircl.[int(Pad.Up)].Disable <- false
            buttonsCircl.[int(Pad.Down)].Disable <- false
            buttonsCircl.[int(Pad.Left)].Disable <- false
            buttonsCircl.[int(Pad.Right)].Disable <- false
            buttonsRot.[int(RotateNav.RotateR)].Disable <- false
            buttonsRot.[int(RotateNav.RotateL)].Disable <- false
            buttonsNav.[int(NavBut.Track)].Disable <- false
            buttonsNav.[int(NavBut.Select)].Disable <- false
            buttonsNav.[int(NavBut.Delete)].Disable <- false
            buttonsNav.[int(NavBut.Animate)].Disable <- false

            if tracks.[tracks.Count - 1].GetTrack.Count > 1 then 
                tracks.[tracks.Count - 1].AddPoint(tracks.[tracks.Count - 1].GetPoint(0))
                tracks.[tracks.Count - 1].DrawingState <- true
                tracks.[tracks.Count - 1].CreatePath()
                tracks.[tracks.Count - 1].GetRect()
                this.LWControls.Add(tracks.[tracks.Count - 1])     
            paintP |> Seq.iter ( fun p ->
               this.LWControls.Remove(p) |> ignore
            )      
            paintP.Clear()
            onceClick <- false
            buttonsNav.[int(NavBut.Track)].Selected <- false
            buttonsNav.[int(NavBut.Track)].Color <- Color.FromArgb(255,102,89);
            drawMode <- false
        
        match dragTrack with
        | Some(b, dx, dy, pp) ->
            b.Selected <- false
            b.PauseCar <- false
        | _ -> ()
        dragTrack <- None
        this.Invalidate()
        base.OnMouseUp(e) 

    override this.OnResize e =
       let mutable x = 64.f
       buttonsNav |> Seq.iter ( fun b ->
              b.Location <- PointF(single(f.ClientSize.Width) - x,single(f.ClientSize.Height) - 64.f)
              x <- x + 64.f
          )
       buttonsRot |> Seq.iter ( fun b ->
              b.Location <- PointF(b.Location.X,single(f.ClientSize.Height) - 64.f)
          )
       buttonsZoom |> Seq.iter ( fun b ->
              b.Location <- PointF(b.Location.X,single(f.ClientSize.Height) - 64.f)
          )

    override this.Dispose e = 
          base.Dispose e
          timerCar.Stop()
    override this.OnPaint e =
        let g = e.Graphics
        g.SmoothingMode <- Drawing2D.SmoothingMode.HighQuality
        let s = g.Save()
        this.W2V <- w2v
        this.V2W <- v2w
        g.Restore(s)  
        base.OnPaint(e)
    override this.OnKeyDown e = 
        if not(buttonsNav.[int(NavBut.Track)].Selected) && not(buttonsNav.[int(NavBut.Select)].Selected) then
            handleCommand e.KeyData
        base.OnKeyDown e 

let c = new MainTrack(Dock=DockStyle.Fill)
f.MinimumSize <- Size(600,600)
f.Controls.Add(c)