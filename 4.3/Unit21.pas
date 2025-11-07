unit Unit21;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, OpenGL, JPEG;

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
    textures: array[0..3] of GLuint;
    procedure SetDCPixelFormat(hdc: HDC);
    procedure InitOpenGL;
    procedure SetupProjection;
    procedure Draw3D;
    procedure LoadJpegTextures;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

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

procedure TForm1.LoadJpegTextures;
var
  i, x, y: Integer;
  jpg: TJPEGImage;
  bmp: TBitmap;
  rgb: array[0..511,0..511,0..2] of GLubyte;
  paths: array[0..3] of string;
begin
  paths[0] := 'C:\Users\Darina\Documents\Embarcadero\Studio\Projects\lab5\1.jpg';
  paths[1] := 'C:\Users\Darina\Documents\Embarcadero\Studio\Projects\lab5\2.jpg';
  paths[2] := 'C:\Users\Darina\Documents\Embarcadero\Studio\Projects\lab5\3.jpg';
  paths[3] := 'C:\Users\Darina\Documents\Embarcadero\Studio\Projects\lab5\4.jpg';

  glGenTextures(4, @textures);

  for i := 0 to 3 do
  begin
    jpg := TJPEGImage.Create;
    bmp := TBitmap.Create;
    try
      jpg.LoadFromFile(paths[i]);
      bmp.Assign(jpg);
      bmp.PixelFormat := pf24bit;
      bmp.Width := 512;
      bmp.Height := 512;

      for y := 0 to 511 do
        for x := 0 to 511 do
        begin
          rgb[y,x,0] := GetRValue(bmp.Canvas.Pixels[x, 511 - y]);
          rgb[y,x,1] := GetGValue(bmp.Canvas.Pixels[x, 511 - y]);
          rgb[y,x,2] := GetBValue(bmp.Canvas.Pixels[x, 511 - y]);
        end;

      glBindTexture(GL_TEXTURE_2D, textures[i]);
      gluBuild2DMipmaps(GL_TEXTURE_2D, 3, 512, 512, GL_RGB, GL_UNSIGNED_BYTE, @rgb);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);
    finally
      jpg.Free;
      bmp.Free;
    end;
  end;
end;

procedure TForm1.InitOpenGL;
begin
  DC := GetDC(Handle);
  SetDCPixelFormat(DC);
  RC := wglCreateContext(DC);
  wglMakeCurrent(DC, RC);

  glClearColor(0.0, 0.0, 0.1, 1.0);
  glEnable(GL_DEPTH_TEST);
  glShadeModel(GL_SMOOTH);

  SetupProjection;
  LoadJpegTextures;
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
  h = 1.0;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;
  glTranslatef(0, 0, -6);
  glRotatef(Angle, 0, 1, 0);
  glEnable(GL_TEXTURE_2D);

  glBindTexture(GL_TEXTURE_2D, textures[0]);
  glBegin(GL_QUADS);
    glTexCoord2f(0,0); glVertex3f(-h, -h, h);
    glTexCoord2f(1,0); glVertex3f( h, -h, h);
    glTexCoord2f(1,1); glVertex3f( h,  h, h);
    glTexCoord2f(0,1); glVertex3f(-h,  h, h);
  glEnd;

  glBindTexture(GL_TEXTURE_2D, textures[1]);
  glBegin(GL_QUADS);
    glTexCoord2f(0,0); glVertex3f( h, -h, h);
    glTexCoord2f(1,0); glVertex3f( h, -h, -h);
    glTexCoord2f(1,1); glVertex3f( h,  h, -h);
    glTexCoord2f(0,1); glVertex3f( h,  h, h);
  glEnd;

  glBindTexture(GL_TEXTURE_2D, textures[2]);
  glBegin(GL_QUADS);
    glTexCoord2f(0,0); glVertex3f( h, -h, -h);
    glTexCoord2f(1,0); glVertex3f(-h, -h, -h);
    glTexCoord2f(1,1); glVertex3f(-h,  h, -h);
    glTexCoord2f(0,1); glVertex3f( h,  h, -h);
  glEnd;

  glBindTexture(GL_TEXTURE_2D, textures[3]);
  glBegin(GL_QUADS);
    glTexCoord2f(0,0); glVertex3f(-h, -h, -h);
    glTexCoord2f(1,0); glVertex3f(-h, -h, h);
    glTexCoord2f(1,1); glVertex3f(-h,  h, h);
    glTexCoord2f(0,1); glVertex3f(-h,  h, -h);
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
    wglMakeCurrent(0,0);
    wglDeleteContext(RC);
  end;
  if DC <> 0 then ReleaseDC(Handle, DC);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  if RC = 0 then Exit;
  glViewport(0,0, ClientWidth, ClientHeight);
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
  if Angle >= 360 then Angle := 0;
  Draw3D;
end;

end.

