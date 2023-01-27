{---------------------------------------------------------------------------------
  X-Tend runtime

  Contributors:
    Vladimir Kustikov (kustikov@sensoft.pro)
    Sergey Arlamenkov (arlamenkov@sensoft.pro)

  You may retrieve the latest version of this file at the GitHub,
  located at https://github.com/sensoftpro/x-tend.git
 ---------------------------------------------------------------------------------
  MIT License

  Copyright © 2023 Sensoft

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 ---------------------------------------------------------------------------------}

unit uVCLPainter;

interface

uses
  uDrawStyles, vclScene;

type
  TVCLScene = class(TWinCanvasScene)
  protected
    function CreatePainter(const AContainer: TObject): TPainter; override;
  end;

implementation

uses
  uModule, uCommonVCLPainter;

{ TVCLScene }

function TVCLScene.CreatePainter(const AContainer: TObject): TPainter;
var
  vContainer: TDrawContainer absolute AContainer;
begin
  Result := TCommonVCLPainter.Create(Self, AContainer);
  FCachedDrawContext := TWinDrawContext(Result.CreateDrawContext(vContainer.Width, vContainer.Height));
  FDrawContext := TWinDrawContext(Result.CreateDrawContext(vContainer.Width, vContainer.Height));
end;

initialization

TBaseModule.RegisterModule('Painting', '', 'VCL', TVCLScene);

end.
