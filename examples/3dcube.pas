program cube;
const Cw = 640;
      Ch = 400;
      Vw = 4;
      Vh = 3;
      D  = 3.5;

      RED = 2;
      GREEN = 3;
      BLUE = 4;

type point2d = record x,y:real; end;
     point3d = record x,y,z:real; end;

var vAf,vBf,vCf,vDf:point3d;
    vAb,vBb,vCb,vDb:point3d;

function viewportToCanvas(x,y:real):point2d;
begin
	viewportToCanvas.x := x * Cw/Vw;
	viewportToCanvas.y := y * Ch/Vh;
end;

function projectVertex(v:point3d):point2d;
begin
	projectVertex := viewportToCanvas(v.x * D / v.z, v.y * D / v.z);
end;

procedure initPoint3d(var p:point3d;x,y,z:real);
begin
	p.x := x;
	p.y := y;
	p.z := z;
end;

procedure DrawLine2d(p1,p2:point2d; color:integer);
begin
        drawline(Cw div 2 + trunc(p1.x),Ch div 2 + trunc(p1.y),
                Cw div 2 + trunc(p2.x), Ch div 2 + trunc(p2.y),
                color);
end;

begin
	initGraphics;

	(* The four "front" vertices *)
	initPoint3d(vAf,-2.0, -0.5, 5.0);
	initPoint3d(vBf,-2.0, 0.5, 5.0);
	initPoint3d(vCf,-1.0,  0.5, 5.0);
	initPoint3d(vDf,-1.0, -0.5, 5.0);

	(* The four "back" vertices *)
	(*
	vAb = [-2, -0.5, 6]
	vBb = [-2,  0.5, 6]
	vCb = [-1,  0.5, 6]
	vDb = [-1, -0.5, 6] *)

	initPoint3d(vAb,-2.0, -0.5, 6.0);
	initPoint3d(vBb,-2.0, 0.5, 6.0);
	initPoint3d(vCb,-1.0,  0.5, 6.0);
	initPoint3d(vDb,-1.0, -0.5, 6.0);

	(* The front face *)
	DrawLine2d(ProjectVertex(vAf), ProjectVertex(vBf), BLUE);
	DrawLine2d(ProjectVertex(vBf), ProjectVertex(vCf), BLUE);
	DrawLine2d(ProjectVertex(vCf), ProjectVertex(vDf), BLUE);
	DrawLine2d(ProjectVertex(vDf), ProjectVertex(vAf), BLUE);

	(* The back face *)
	DrawLine2d(ProjectVertex(vAb), ProjectVertex(vBb), RED);
	DrawLine2d(ProjectVertex(vBb), ProjectVertex(vCb), RED);
	DrawLine2d(ProjectVertex(vCb), ProjectVertex(vDb), RED);
	DrawLine2d(ProjectVertex(vDb), ProjectVertex(vAb), RED);

	(* The front-to-back edges *)
	DrawLine2d(ProjectVertex(vAf), ProjectVertex(vAb), GREEN);
	DrawLine2d(ProjectVertex(vBf), ProjectVertex(vBb), GREEN);
	DrawLine2d(ProjectVertex(vCf), ProjectVertex(vCb), GREEN);
	DrawLine2d(ProjectVertex(vDf), ProjectVertex(vDb), GREEN);
end.
