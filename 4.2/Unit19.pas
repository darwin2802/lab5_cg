unit Unit19;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, OpenGL, Math;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    DC: HDC;
    RC: HGLRC;
    Angle: GLfloat;
    procedure SetDCPixelFormat(hdc: HDC);
    procedure InitOpenGL;
    procedure SetupProjection;
    procedure LoadBmpTexture(const FileName: string);
    procedure Draw3D;
  public
  end;

var
  Form1: TForm1;
  arrayRGB: array[0..511, 0..2047, 0..2] of GLubyte;

implementation

{$R *.dfm}

const
  tw = 2048;
  th = 512;

procedure TForm1.SetDCPixelFormat(hdc: HDC);
var
  pfd: TPixelFormatDescriptor;
  pf: Integer;
begin
  FillChar(pfd, SizeOf(pfd), 0);
  pfd.nSize := SizeOf(pfd);
  pfd.nVersion := 1;
  pfd.dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
  pfd.iPixelType := PFD_TYPE_RGBA;
  pfd.cColorBits := 24;
  pfd.cDepthBits := 16;
  pfd.iLayerType := PFD_MAIN_PLANE;
  pf := ChoosePixelFormat(hdc, @pfd);
  SetPixelFormat(hdc, pf, @pfd);
end;

procedure TForm1.LoadBmpTexture(const FileName: string);
var
  Bmp: TBitmap;
  i, j: Integer;
  c: TColor;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.LoadFromFile(FileName);
    Bmp.PixelFormat := pf24bit;

    for i := 0 to tw - 1 do
      for j := 0 to th - 1 do
      begin
        c := Bmp.Canvas.Pixels[i, th - 1 - j];
        arrayRGB[j, i, 0] := GetRValue(c);
        arrayRGB[j, i, 1] := GetGValue(c);
        arrayRGB[j, i, 2] := GetBValue(c);
      end;

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, tw, th, 0,
      GL_RGB, GL_UNSIGNED_BYTE, @arrayRGB);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  finally
    Bmp.Free;
  end;
end;

procedure TForm1.InitOpenGL;
begin
  DC := GetDC(Handle);
  SetDCPixelFormat(DC);
  RC := wglCreateContext(DC);
  wglMakeCurrent(DC, RC);

  glClearColor(0.0, 0.0, 0.15, 1.0);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_TEXTURE_2D);
  glShadeModel(GL_SMOOTH);
  SetupProjection;

  LoadBmpTexture('C:\Users\Darina\Documents\Embarcadero\Studio\Projects\lab5\panoram.bmp');
end;

procedure TForm1.SetupProjection;
var
  aspect: GLfloat;
begin
  if ClientHeight = 0 then aspect := 1
  else aspect := ClientWidth / ClientHeight;

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(45.0, aspect, 1.0, 100.0);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

procedure TForm1.Draw3D;
const
  n = 8;
  h = 2.0;
  R = 0.8;
var
  i: Integer;
  fi, delta_fi, s: GLfloat;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;
  glTranslatef(0.0, 0.0, -6.0);
  glRotatef(Angle, 0, 1, 0);

  s := -Angle / 360;
  delta_fi := 2 * Pi / n;
  fi := 0.0;

  glBegin(GL_QUAD_STRIP);
  for i := 0 to n do
  begin
    glTexCoord2d(i / n + s, 1);
    glVertex3f(R * Cos(fi), h / 2, R * Sin(fi));
    glTexCoord2d(i / n + s, 0);
    glVertex3f(R * Cos(fi), -h / 2, R * Sin(fi));
    fi := fi + delta_fi;
  end;
  glEnd;

  SwapBuffers(DC);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitOpenGL;
  Angle := 0;
  Timer1.Interval := 16;
  Timer1.Enabled := True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if RC <> 0 then
  begin
    wglMakeCurrent(0, 0);
    wglDeleteContext(RC);
    RC := 0;
  end;
  if DC <> 0 then
    ReleaseDC(Handle, DC);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  if RC = 0 then Exit;
  glViewport(0, 0, ClientWidth, ClientHeight);
  SetupProjection;
  Draw3D;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  Draw3D;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Angle := Angle + 1;
  if Angle > 360 then Angle := 0;
  Draw3D;
end;

end.

