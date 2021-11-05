{---------------------------------------------------------------------------------
  X-Tend runtime

  Contributors:
    Vladimir Kustikov (kustikov@sensoft.pro)
    Sergey Arlamenkov (arlamenkov@sensoft.pro)

  You may retrieve the latest version of this file at the GitHub,
  located at https://github.com/sensoftpro/x-tend.git
 ---------------------------------------------------------------------------------
  MIT License

  Copyright © 2021 Sensoft

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

unit uD2DExtra;

interface

uses
  System.Types,
  WinApi.Windows,
  WinApi.DXTypes,
  WinApi.DxgiType,
  Winapi.DxgiFormat,
  WinApi.D2D1,
  Winapi.ActiveX,
  Winapi.Wincodec,
  WinApi.DXGI;

const
  SID_IWICImagingFactory2 = '{7B816B45-1996-4476-B132-DE9E247C8AF0}';
  IID_IWICImagingFactory2: TGUID = SID_IWICImagingFactory2;
  {$EXTERNALSYM IID_IWICImagingFactory2}

type
//  // Represents a 3-by-2 matrix.
//  // Delphi Note:
//  PD2D_MATRIX_3X2_F = ^D2D_MATRIX_3X2_F;
//  D2D_MATRIX_3X2_F = record
//    // Horizontal scaling / cosine of rotation
//    _11: Single;
//    // Vertical shear / sine of rotation
//    _12: Single;
//
//    // Horizontal shear / negative sine of rotation
//    _21: Single;
//    // Vertical scaling / cosine of rotation
//    _22: Single;
//
//    // Horizontal shift (always orthogonal regardless of rotation)
//    _31: Single;
//    // Vertical shift (always orthogonal regardless of rotation)
//    _32: Single;
//  end;
//  PD2D1_MATRIX_3X2_F = ^D2D1_MATRIX_3X2_F;
//  D2D1_MATRIX_3X2_F = D2D_MATRIX_3X2_F;
//  {$EXTERNALSYM D2D1_MATRIX_3X2_F}

  // Represents a 4-by-4 matrix.
  PD2D_MATRIX_4X4_F = ^D2D_MATRIX_4X4_F;
  D2D_MATRIX_4X4_F = record
    _11: Single;
    _12: Single;
    _13: Single;
    _14: Single;

    _21: Single;
    _22: Single;
    _23: Single;
    _24: Single;

    _31: Single;
    _32: Single;
    _33: Single;
    _34: Single;

    _41: Single;
    _42: Single;
    _43: Single;
    _44: Single;
  end;
  {$EXTERNALSYM D2D_MATRIX_4X4_F}
  PD2D1_MATRIX_4X4_F = ^D2D_MATRIX_4X4_F;
  D2D1_MATRIX_4X4_F = D2D_MATRIX_4X4_F;
  {$EXTERNALSYM D2D1_MATRIX_4X4_F}

//  {$IF CompilerVersion <= 30}
  DXGI_COLOR_SPACE_TYPE = WORD;
//  {$IFEND}

  // Fonts may contain multiple drawable data formats for glyphs. These flags specify which formats
  // are supported in the font, either at a font-wide level or per glyph, and the app may use them
  // to tell DWrite which formats to return when splitting a color glyph run.
  PDWRITE_GLYPH_IMAGE_FORMATS = ^DWRITE_GLYPH_IMAGE_FORMATS;
  DWRITE_GLYPH_IMAGE_FORMATS = DWord;
  {$EXTERNALSYM DWRITE_GLYPH_IMAGE_FORMATS}
const
  {$IF CompilerVersion < 33}
  D2D1_DRAW_TEXT_OPTIONS_ENABLE_COLOR_FONT = $00000004; // Render color versions of glyphs if defined by the font.
  {$ENDIF}
  // Indicates no data is available for this glyph.
  DWRITE_GLYPH_IMAGE_FORMATS_NONE                   = DWRITE_GLYPH_IMAGE_FORMATS($00000000);
  {$EXTERNALSYM DWRITE_GLYPH_IMAGE_FORMATS_NONE}
  // The glyph has TrueType outlines.
  DWRITE_GLYPH_IMAGE_FORMATS_TRUETYPE               = DWRITE_GLYPH_IMAGE_FORMATS($00000001);
  {$EXTERNALSYM DWRITE_GLYPH_IMAGE_FORMATS_TRUETYPE}
  // The glyph has CFF outlines.
  DWRITE_GLYPH_IMAGE_FORMATS_CFF                    = DWRITE_GLYPH_IMAGE_FORMATS($00000002);
  {$EXTERNALSYM DWRITE_GLYPH_IMAGE_FORMATS_CFF}
  // The glyph has multilayered COLR data.
  DWRITE_GLYPH_IMAGE_FORMATS_COLR                   = DWRITE_GLYPH_IMAGE_FORMATS($00000004);
  {$EXTERNALSYM DWRITE_GLYPH_IMAGE_FORMATS_COLR}
  // The glyph has SVG outlines as standard XML.
  // <remarks>
  // Fonts may store the content gzip'd rather than plain text);
  // indicated by the first two bytes as gzip header {0x1F 0x8B}.
  // </remarks>
  DWRITE_GLYPH_IMAGE_FORMATS_SVG                    = DWRITE_GLYPH_IMAGE_FORMATS($00000008);
  {$EXTERNALSYM DWRITE_GLYPH_IMAGE_FORMATS_SVG}
  // The glyph has PNG image data); with standard PNG IHDR.
  DWRITE_GLYPH_IMAGE_FORMATS_PNG                    = DWRITE_GLYPH_IMAGE_FORMATS($00000010);
  {$EXTERNALSYM DWRITE_GLYPH_IMAGE_FORMATS_PNG}
  // The glyph has JPEG image data); with standard JIFF SOI header.
  DWRITE_GLYPH_IMAGE_FORMATS_JPEG                   = DWRITE_GLYPH_IMAGE_FORMATS($00000020);
  {$EXTERNALSYM DWRITE_GLYPH_IMAGE_FORMATS_JPEG}
  // The glyph has TIFF image data.
  DWRITE_GLYPH_IMAGE_FORMATS_TIFF                   = DWRITE_GLYPH_IMAGE_FORMATS($00000040);
  {$EXTERNALSYM DWRITE_GLYPH_IMAGE_FORMATS_TIFF}
  // The glyph has raw 32-bit premultiplied BGRA data.
  DWRITE_GLYPH_IMAGE_FORMATS_PREMULTIPLIED_B8G8R8A8 = DWRITE_GLYPH_IMAGE_FORMATS($00000080);
  {$EXTERNALSYM DWRITE_GLYPH_IMAGE_FORMATS_PREMULTIPLIED_B8G8R8A8}

type
  // A blend mode that applies to all primitives drawn on the context.
  PD2D1_PRIMITIVE_BLEND = ^D2D1_PRIMITIVE_BLEND;
  D2D1_PRIMITIVE_BLEND = DWord;
  {$EXTERNALSYM D2D1_PRIMITIVE_BLEND}
const
  D2D1_PRIMITIVE_BLEND_SOURCE_OVER = D2D1_PRIMITIVE_BLEND(0);
  D2D1_PRIMITIVE_BLEND_COPY        = D2D1_PRIMITIVE_BLEND(1);
  D2D1_PRIMITIVE_BLEND_MIN         = D2D1_PRIMITIVE_BLEND(2);
  D2D1_PRIMITIVE_BLEND_ADD         = D2D1_PRIMITIVE_BLEND(3);
  D2D1_PRIMITIVE_BLEND_MAX         = D2D1_PRIMITIVE_BLEND(4);
  //D2D1_PRIMITIVE_BLEND_FORCE_DWORD = FORCEDWORD;

type
  // This is used to specify the quality of image scaling with
  // ID2D1DeviceContext.DrawImage and with the 2D Affine Transform Effect.
  PD2D1_INTERPOLATION_MODE = ^D2D1_INTERPOLATION_MODE;
  D2D1_INTERPOLATION_MODE = DWord;
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE}
const
  D2D1_INTERPOLATION_MODE_NEAREST_NEIGHBOR    = 0;
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE_NEAREST_NEIGHBOR}
  D2D1_INTERPOLATION_MODE_LINEAR              = 1;
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE_LINEAR}
  D2D1_INTERPOLATION_MODE_CUBIC               = 2;
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE_CUBIC}
  D2D1_INTERPOLATION_MODE_MULTI_SAMPLE_LINEAR =3;
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE_MULTI_SAMPLE_LINEAR}
  D2D1_INTERPOLATION_MODE_ANISOTROPIC         = 4;
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE_ANISOTROPIC}
  D2D1_INTERPOLATION_MODE_HIGH_QUALITY_CUBIC  = 5;
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE_HIGH_QUALITY_CUBIC}
  D2D1_INTERPOLATION_MODE_DEFINITION_FANT = 6;
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE_DEFINITION_FANT}
  D2D1_INTERPOLATION_MODE_DEFINITION_MIPMAP_LINEAR = 7;
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE_DEFINITION_MIPMAP_LINEAR}
  //D2D1_INTERPOLATION_MODE_FORCE_DWORD         = FORCEDWORD;

type
  // This specifies what units should be accepted by the D2D API.
  PD2D1_UNIT_MODE = ^D2D1_UNIT_MODE;
  D2D1_UNIT_MODE = DWord;
  {$EXTERNALSYM D2D1_UNIT_MODE}
const
  D2D1_UNIT_MODE_DIPS        = D2D1_UNIT_MODE(0);
  {$EXTERNALSYM D2D1_UNIT_MODE_DIPS}
  D2D1_UNIT_MODE_PIXELS      = D2D1_UNIT_MODE(1);
  {$EXTERNALSYM D2D1_UNIT_MODE_PIXELS}
  //D2D1_UNIT_MODE_FORCE_DWORD = FORCEDWORD;

type
  // Defines a color space.
  PD2D1_COLOR_SPACE = ^D2D1_COLOR_SPACE;
  D2D1_COLOR_SPACE = DWord;
  {$EXTERNALSYM D2D1_COLOR_SPACE}

  // Specifies which way a color profile is defined.
  PD2D1_COLOR_CONTEXT_TYPE = ^D2D1_COLOR_CONTEXT_TYPE;
  D2D1_COLOR_CONTEXT_TYPE = DWord;
  {$EXTERNALSYM D2D1_COLOR_CONTEXT_TYPE}

type
  // Specifies the pixel snapping policy when rendering color bitmap glyphs.
  PD2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION = ^D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION;
  D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION = DWord;
  {$EXTERNALSYM D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION}
const
  // Color bitmap glyph positions are snapped to the nearest pixel if the bitmap
  // resolution matches that of the device context.
  D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION_DEFAULT   = D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION(0);
  {$EXTERNALSYM D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION_DEFAULT}
  // Color bitmap glyph positions are not snapped.
  D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION_DISABLE   = D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION(1);
  {$EXTERNALSYM D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION_DISABLE}
  //D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION_FORCE_DWORD = FORCEDWORD;

type
  // This determines what gamma is used for interpolation/blending.
  PD2D1_GAMMA1 = ^D2D1_GAMMA1;
  D2D1_GAMMA1 = DWord;
  {$EXTERNALSYM D2D1_GAMMA1}

const
  // Colors are manipulated in 2.2 gamma color space.
  D2D1_GAMMA1_G22     = D2D1_GAMMA_2_2;
  // Colors are manipulated in 1.0 gamma color space.
  D2D1_GAMMA1_G10     = D2D1_GAMMA_1_0;
  // Colors are manipulated in ST.2084 PQ gamma color space.
  D2D1_GAMMA1_G2084     = D2D1_GAMMA1(2);
  //D2D1_GAMMA1_FORCE_DWORD = FORCEDWORD;

  D2D1_COLOR_CONTEXT_TYPE_ICC     = D2D1_COLOR_CONTEXT_TYPE(0);
  {$EXTERNALSYM D2D1_COLOR_CONTEXT_TYPE_ICC}
  D2D1_COLOR_CONTEXT_TYPE_SIMPLE  = D2D1_COLOR_CONTEXT_TYPE(1);
  {$EXTERNALSYM D2D1_COLOR_CONTEXT_TYPE_SIMPLE}
  D2D1_COLOR_CONTEXT_TYPE_DXGI    = D2D1_COLOR_CONTEXT_TYPE(2);
  {$EXTERNALSYM D2D1_COLOR_CONTEXT_TYPE_DXGI}
  //D2D1_COLOR_CONTEXT_TYPE_FORCE_DWORD = FORCEDWORD;

type
  // Specifies the composite mode that will be applied.
  PD2D1_COMPOSITE_MODE = ^D2D1_COMPOSITE_MODE;
  D2D1_COMPOSITE_MODE = DWord;
  {$EXTERNALSYM D2D1_COMPOSITE_MODE}
const
  D2D1_COMPOSITE_MODE_SOURCE_OVER         = D2D1_COMPOSITE_MODE(0);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_SOURCE_OVER}
  D2D1_COMPOSITE_MODE_DESTINATION_OVER    = D2D1_COMPOSITE_MODE(1);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_DESTINATION_OVER}
  D2D1_COMPOSITE_MODE_SOURCE_IN           = D2D1_COMPOSITE_MODE(2);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_SOURCE_IN}
  D2D1_COMPOSITE_MODE_DESTINATION_IN      = D2D1_COMPOSITE_MODE(3);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_DESTINATION_IN}
  D2D1_COMPOSITE_MODE_SOURCE_OUT          = D2D1_COMPOSITE_MODE(4);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_SOURCE_OUT}
  D2D1_COMPOSITE_MODE_DESTINATION_OUT     = D2D1_COMPOSITE_MODE(5);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_DESTINATION_OUT}
  D2D1_COMPOSITE_MODE_SOURCE_ATOP         = D2D1_COMPOSITE_MODE(6);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_SOURCE_ATOP}
  D2D1_COMPOSITE_MODE_DESTINATION_ATOP    = D2D1_COMPOSITE_MODE(7);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_DESTINATION_ATOP}
  D2D1_COMPOSITE_MODE_XOR                 = D2D1_COMPOSITE_MODE(8);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_XOR}
  D2D1_COMPOSITE_MODE_PLUS                = D2D1_COMPOSITE_MODE(9);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_PLUS}
  D2D1_COMPOSITE_MODE_SOURCE_COPY         = D2D1_COMPOSITE_MODE(10);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_SOURCE_COPY}
  D2D1_COMPOSITE_MODE_BOUNDED_SOURCE_COPY = D2D1_COMPOSITE_MODE(11);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_BOUNDED_SOURCE_COPY}
  D2D1_COMPOSITE_MODE_MASK_INVERT         = D2D1_COMPOSITE_MODE(12);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_MASK_INVERT}
  //D2D1_COMPOSITE_MODE_FORCE_DWORD         = FORCEDWORD;

type
  // Option flags controlling how images sources are loaded during
  // CreateImageSourceFromWic.
  PD2D1_IMAGE_SOURCE_LOADING_OPTIONS = ^D2D1_IMAGE_SOURCE_LOADING_OPTIONS;
  D2D1_IMAGE_SOURCE_LOADING_OPTIONS = DWord;
  {$EXTERNALSYM D2D1_IMAGE_SOURCE_LOADING_OPTIONS}
const
  {$EXTERNALSYM D2D1_IMAGE_SOURCE_LOADING_OPTIONS_NONE}
  D2D1_IMAGE_SOURCE_LOADING_OPTIONS_NONE            = D2D1_IMAGE_SOURCE_LOADING_OPTIONS(0);
  {$EXTERNALSYM D2D1_IMAGE_SOURCE_LOADING_OPTIONS_RELEASE_SOURCE}
  D2D1_IMAGE_SOURCE_LOADING_OPTIONS_RELEASE_SOURCE  = D2D1_IMAGE_SOURCE_LOADING_OPTIONS(1);
  {$EXTERNALSYM D2D1_IMAGE_SOURCE_LOADING_OPTIONS_CACHE_ON_DEMAND}
  D2D1_IMAGE_SOURCE_LOADING_OPTIONS_CACHE_ON_DEMAND = D2D1_IMAGE_SOURCE_LOADING_OPTIONS(2);
  //D2D1_IMAGE_SOURCE_LOADING_OPTIONS_FORCE_DWORD   = FORCEDWORD;

type
  // Specifies the orientation of an image.
  PD2D1_ORIENTATION = ^D2D1_ORIENTATION;
  D2D1_ORIENTATION = DWord;
  {$EXTERNALSYM D2D1_ORIENTATION}
const
  D2D1_ORIENTATION_DEFAULT                             = D2D1_ORIENTATION(1);
  {$EXTERNALSYM D2D1_ORIENTATION_DEFAULT}
  D2D1_ORIENTATION_FLIP_HORIZONTAL                     = D2D1_ORIENTATION(2);
  {$EXTERNALSYM D2D1_ORIENTATION_FLIP_HORIZONTAL}
  D2D1_ORIENTATION_ROTATE_CLOCKWISE180                 = D2D1_ORIENTATION(3);
  {$EXTERNALSYM D2D1_ORIENTATION_ROTATE_CLOCKWISE180}
  D2D1_ORIENTATION_ROTATE_CLOCKWISE180_FLIP_HORIZONTAL = D2D1_ORIENTATION(4);
  {$EXTERNALSYM D2D1_ORIENTATION_ROTATE_CLOCKWISE180_FLIP_HORIZONTAL}
  D2D1_ORIENTATION_ROTATE_CLOCKWISE90_FLIP_HORIZONTAL  = D2D1_ORIENTATION(5);
  {$EXTERNALSYM D2D1_ORIENTATION_ROTATE_CLOCKWISE90_FLIP_HORIZONTAL}
  D2D1_ORIENTATION_ROTATE_CLOCKWISE270                 = D2D1_ORIENTATION(6);
  {$EXTERNALSYM D2D1_ORIENTATION_ROTATE_CLOCKWISE270}
  D2D1_ORIENTATION_ROTATE_CLOCKWISE270_FLIP_HORIZONTAL = D2D1_ORIENTATION(7);
  {$EXTERNALSYM D2D1_ORIENTATION_ROTATE_CLOCKWISE270_FLIP_HORIZONTAL}
  D2D1_ORIENTATION_ROTATE_CLOCKWISE90                  = D2D1_ORIENTATION(8);
  {$EXTERNALSYM D2D1_ORIENTATION_ROTATE_CLOCKWISE90}
  //D2D1_ORIENTATION_FORCE_DWORD             = FORCEDWORD;

type
  // Option flags for transformed image sources.
  PD2D1_TRANSFORMED_IMAGE_SOURCE_OPTIONS = ^D2D1_TRANSFORMED_IMAGE_SOURCE_OPTIONS;
  D2D1_TRANSFORMED_IMAGE_SOURCE_OPTIONS = DWord;
  {$EXTERNALSYM D2D1_TRANSFORMED_IMAGE_SOURCE_OPTIONS}
const
  D2D1_TRANSFORMED_IMAGE_SOURCE_OPTIONS_NONE              = D2D1_TRANSFORMED_IMAGE_SOURCE_OPTIONS(0);
  {$EXTERNALSYM D2D1_TRANSFORMED_IMAGE_SOURCE_OPTIONS_NONE}
  // Prevents the image source from being automatically scaled (by a ratio of the
  // context DPI divided by 96) while drawn.
  D2D1_TRANSFORMED_IMAGE_SOURCE_OPTIONS_DISABLE_DPI_SCALE = D2D1_TRANSFORMED_IMAGE_SOURCE_OPTIONS(1);
  {$EXTERNALSYM D2D1_TRANSFORMED_IMAGE_SOURCE_OPTIONS_DISABLE_DPI_SCALE}
  //D2D1_TRANSFORMED_IMAGE_SOURCE_OPTIONS_FORCE_DWORD     = FORCEDWORD;

type
  // Option flags controlling primary conversion performed by
  // CreateImageSourceFromDxgi); if any.
  PD2D1_IMAGE_SOURCE_FROM_DXGI_OPTIONS = ^D2D1_IMAGE_SOURCE_FROM_DXGI_OPTIONS;
  D2D1_IMAGE_SOURCE_FROM_DXGI_OPTIONS = DWord;
  {$EXTERNALSYM D2D1_IMAGE_SOURCE_FROM_DXGI_OPTIONS}
const
  D2D1_IMAGE_SOURCE_FROM_DXGI_OPTIONS_NONE                           = D2D1_IMAGE_SOURCE_FROM_DXGI_OPTIONS(0);
  {$EXTERNALSYM D2D1_IMAGE_SOURCE_FROM_DXGI_OPTIONS_NONE}
  D2D1_IMAGE_SOURCE_FROM_DXGI_OPTIONS_LOW_QUALITY_PRIMARY_CONVERSION = D2D1_IMAGE_SOURCE_FROM_DXGI_OPTIONS(1);
  {$EXTERNALSYM D2D1_IMAGE_SOURCE_FROM_DXGI_OPTIONS_LOW_QUALITY_PRIMARY_CONVERSION}
  //D2D1_IMAGE_SOURCE_FROM_DXGI_OPTIONS_FORCE_DWORD          = FORCEDWORD;

type
  // Simple description of a color space.
  PD2D1_SIMPLE_COLOR_PROFILE = ^D2D1_SIMPLE_COLOR_PROFILE;
  D2D1_SIMPLE_COLOR_PROFILE = record
    // The XY coordinates of the red primary in CIEXYZ space.
    redPrimary: D2D1_POINT_2F;
    // The XY coordinates of the green primary in CIEXYZ space.
    greenPrimary: D2D1_POINT_2F;
    // The XY coordinates of the blue primary in CIEXYZ space.
    bluePrimary: D2D1_POINT_2F;
    // The X/Z tristimulus values for the whitepoint, normalized for relative
    // luminance.
    whitePointXZ: D2D1_POINT_2F;
    // The gamma encoding to use for this color space.
    gamma: D2D1_GAMMA1;
  end;
  {$EXTERNALSYM D2D1_SIMPLE_COLOR_PROFILE}

  // D2D1_LAYER_OPTIONS1
type
  // Specifies how the layer contents should be prepared.
  PD2D1_LAYER_OPTIONS1 = ^D2D1_LAYER_OPTIONS1;
  D2D1_LAYER_OPTIONS1 = Dword;
  {$EXTERNALSYM D2D1_LAYER_OPTIONS1}
const
  D2D1_LAYER_OPTIONS1_NONE                       = D2D1_LAYER_OPTIONS1(0);
  D2D1_LAYER_OPTIONS1_INITIALIZE_FROM_BACKGROUND = D2D1_LAYER_OPTIONS1(1);
  D2D1_LAYER_OPTIONS1_IGNORE_ALPHA               = D2D1_LAYER_OPTIONS1(2);
  //D2D1_LAYER_OPTIONS1_FORCE_DWORD                = FORCEDWORD;

type
  PD2D1_SPRITE_OPTIONS = ^D2D1_SPRITE_OPTIONS;
  D2D1_SPRITE_OPTIONS = DWord;
  {$EXTERNALSYM D2D1_SPRITE_OPTIONS}
const
  // Use default sprite rendering behavior.
  D2D1_SPRITE_OPTIONS_NONE                      = D2D1_SPRITE_OPTIONS(0);
  {$EXTERNALSYM D2D1_SPRITE_OPTIONS_NONE}
  // Bitmap interpolation will be clamped to the sprite's source rectangle.
  D2D1_SPRITE_OPTIONS_CLAMP_TO_SOURCE_RECTANGLE = D2D1_SPRITE_OPTIONS(1);
  {$EXTERNALSYM D2D1_SPRITE_OPTIONS_CLAMP_TO_SOURCE_RECTANGLE}
  //D2D1_SPRITE_OPTIONS_FORCE_DWORD         = FORCEDWORD;

type
  // This specifies the precision that should be used in buffers allocated by D2D.
  PD2D1_BUFFER_PRECISION = ^D2D1_BUFFER_PRECISION;
  D2D1_BUFFER_PRECISION = DWord;
  {$EXTERNALSYM D2D1_BUFFER_PRECISION}
const
  D2D1_BUFFER_PRECISION_UNKNOWN         = D2D1_BUFFER_PRECISION(0);
  {$EXTERNALSYM D2D1_BUFFER_PRECISION_UNKNOWN}
  D2D1_BUFFER_PRECISION_8BPC_UNORM      = D2D1_BUFFER_PRECISION(1);
  {$EXTERNALSYM D2D1_BUFFER_PRECISION_8BPC_UNORM}
  D2D1_BUFFER_PRECISION_8BPC_UNORM_SRGB = D2D1_BUFFER_PRECISION(2);
  {$EXTERNALSYM D2D1_BUFFER_PRECISION_8BPC_UNORM_SRGB}
  D2D1_BUFFER_PRECISION_16BPC_UNORM     = D2D1_BUFFER_PRECISION(3);
  {$EXTERNALSYM D2D1_BUFFER_PRECISION_16BPC_UNORM}
  D2D1_BUFFER_PRECISION_16BPC_FLOAT     = D2D1_BUFFER_PRECISION(4);
  {$EXTERNALSYM D2D1_BUFFER_PRECISION_16BPC_FLOAT}
  D2D1_BUFFER_PRECISION_32BPC_FLOAT     = D2D1_BUFFER_PRECISION(5);
  {$EXTERNALSYM D2D1_BUFFER_PRECISION_32BPC_FLOAT}
  //D2D1_BUFFER_PRECISION_FORCE_DWORD     = FORCEDWORD;

type
  // Specifies how the bitmap can be used.
  PD2D1_BITMAP_OPTIONS = ^D2D1_BITMAP_OPTIONS;
  D2D1_BITMAP_OPTIONS = DWord;
  {$EXTERNALSYM D2D1_BITMAP_OPTIONS}
const
  // The bitmap is created with default properties.
  D2D1_BITMAP_OPTIONS_NONE           = D2D1_BITMAP_OPTIONS($00000000);
  // The bitmap can be specified as a target in ID2D1DeviceContext.SetTarget
  D2D1_BITMAP_OPTIONS_TARGET         = D2D1_BITMAP_OPTIONS($00000001);
  // The bitmap cannot be used as an input to DrawBitmap, DrawImage, in a bitmap
  // brush or as an input to an effect.
  D2D1_BITMAP_OPTIONS_CANNOT_DRAW    = D2D1_BITMAP_OPTIONS($00000002);
  // The bitmap can be read from the CPU.
  D2D1_BITMAP_OPTIONS_CPU_READ       = D2D1_BITMAP_OPTIONS($00000004);
  // The bitmap works with the ID2D1GdiInteropRenderTarget.GetDC API.
  D2D1_BITMAP_OPTIONS_GDI_COMPATIBLE = D2D1_BITMAP_OPTIONS($00000008);
  // D2D1_BITMAP_OPTIONS_FORCE_DWORD = FORCEDWORD;

type
  // This describes how the individual mapping operation should be performed.
  PD2D1_MAP_OPTIONS = ^D2D1_MAP_OPTIONS;
  D2D1_MAP_OPTIONS = DWord;
  {$EXTERNALSYM D2D1_MAP_OPTIONS}
const
  // The mapped pointer has undefined behavior.
  D2D1_MAP_OPTIONS_NONE        = D2D1_MAP_OPTIONS(0);
  {$EXTERNALSYM D2D1_MAP_OPTIONS_NONE}

  // The mapped pointer can be read from.
  D2D1_MAP_OPTIONS_READ        = D2D1_MAP_OPTIONS(1);
  {$EXTERNALSYM D2D1_MAP_OPTIONS_READ}

  // The mapped pointer can be written to.
  D2D1_MAP_OPTIONS_WRITE       = D2D1_MAP_OPTIONS(2);
  {$EXTERNALSYM D2D1_MAP_OPTIONS_WRITE}

  // The previous contents of the bitmap are discarded when it is mapped.
  D2D1_MAP_OPTIONS_DISCARD     = D2D1_MAP_OPTIONS(4);
  {$EXTERNALSYM D2D1_MAP_OPTIONS_DISCARD}
  //D2D1_MAP_OPTIONS_FORCE_DWORD = FORCEDWORD;


type
  // This enum defines the valid property types that can be used in an effect property
  // interface.
  PD2D1_PROPERTY_TYPE = ^D2D1_PROPERTY_TYPE;
  D2D1_PROPERTY_TYPE = DWord;
  {$EXTERNALSYM D2D1_PROPERTY_TYPE}
const
  D2D1_PROPERTY_TYPE_UNKNOWN       = D2D1_PROPERTY_TYPE(0);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_UNKNOWN}
  D2D1_PROPERTY_TYPE_STRING        = D2D1_PROPERTY_TYPE(1);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_STRING}
  D2D1_PROPERTY_TYPE_BOOL          = D2D1_PROPERTY_TYPE(2);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_BOOL}
  D2D1_PROPERTY_TYPE_UINT32        = D2D1_PROPERTY_TYPE(3);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_UINT32}
  D2D1_PROPERTY_TYPE_INT32         = D2D1_PROPERTY_TYPE(4);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_INT32}
  D2D1_PROPERTY_TYPE_FLOAT         = D2D1_PROPERTY_TYPE(5);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_FLOAT}
  D2D1_PROPERTY_TYPE_VECTOR2       = D2D1_PROPERTY_TYPE(6);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_VECTOR2}
  D2D1_PROPERTY_TYPE_VECTOR3       = D2D1_PROPERTY_TYPE(7);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_VECTOR3}
  D2D1_PROPERTY_TYPE_VECTOR4       = D2D1_PROPERTY_TYPE(8);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_VECTOR4}
  D2D1_PROPERTY_TYPE_BLOB          = D2D1_PROPERTY_TYPE(9);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_BLOB}
  D2D1_PROPERTY_TYPE_IUNKNOWN      = D2D1_PROPERTY_TYPE(10);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_IUNKNOWN}
  D2D1_PROPERTY_TYPE_ENUM          = D2D1_PROPERTY_TYPE(11);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_ENUM}
  D2D1_PROPERTY_TYPE_ARRAY         = D2D1_PROPERTY_TYPE(12);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_ARRAY}
  D2D1_PROPERTY_TYPE_CLSID         = D2D1_PROPERTY_TYPE(13);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_CLSID}
  D2D1_PROPERTY_TYPE_MATRIX_3X2    = D2D1_PROPERTY_TYPE(14);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_MATRIX_3X2}
  D2D1_PROPERTY_TYPE_MATRIX_4X3    = D2D1_PROPERTY_TYPE(15);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_MATRIX_4X3}
  D2D1_PROPERTY_TYPE_MATRIX_4X4    = D2D1_PROPERTY_TYPE(16);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_MATRIX_4X4}
  D2D1_PROPERTY_TYPE_MATRIX_5X4    = D2D1_PROPERTY_TYPE(17);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_MATRIX_5X4}
  D2D1_PROPERTY_TYPE_COLOR_CONTEXT = D2D1_PROPERTY_TYPE(18);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_COLOR_CONTEXT}
  //D2D1_PROPERTY_TYPE_FORCE_DWORD   = FORCEDWORD;

type
  // This defines the list of system properties present on the root effect property
  // interface.
  PD2D1_PROPERTY = ^D2D1_PROPERTY;
  D2D1_PROPERTY = DWord;
  {$EXTERNALSYM D2D1_PROPERTY}
const
  D2D1_PROPERTY_CLSID       = D2D1_PROPERTY(0);
  {$EXTERNALSYM D2D1_PROPERTY_CLSID}
  D2D1_PROPERTY_DISPLAYNAME = D2D1_PROPERTY(1);
  {$EXTERNALSYM D2D1_PROPERTY_DISPLAYNAME}
  D2D1_PROPERTY_AUTHOR      = D2D1_PROPERTY(2);
  {$EXTERNALSYM D2D1_PROPERTY_AUTHOR}
  D2D1_PROPERTY_CATEGORY    = D2D1_PROPERTY(3);
  {$EXTERNALSYM D2D1_PROPERTY_CATEGORY}
  D2D1_PROPERTY_DESCRIPTION = D2D1_PROPERTY(4);
  {$EXTERNALSYM D2D1_PROPERTY_DESCRIPTION}
  D2D1_PROPERTY_INPUTS      = D2D1_PROPERTY(5);
  {$EXTERNALSYM D2D1_PROPERTY_INPUTS}
  D2D1_PROPERTY_CACHED      = D2D1_PROPERTY(6);
  {$EXTERNALSYM D2D1_PROPERTY_CACHED}
  D2D1_PROPERTY_PRECISION   = D2D1_PROPERTY(7);
  {$EXTERNALSYM D2D1_PROPERTY_PRECISION}
  D2D1_PROPERTY_MIN_INPUTS  = D2D1_PROPERTY(8);
  {$EXTERNALSYM D2D1_PROPERTY_MIN_INPUTS}
  D2D1_PROPERTY_MAX_INPUTS  = D2D1_PROPERTY(9);
  {$EXTERNALSYM D2D1_PROPERTY_MAX_INPUTS}
  //D2D1_PROPERTY_FORCE_DWORD = FORCEDWORD;

type
  // This defines the indices of sub-properties that may be present on any parent
  // property.
  PD2D1_SUBPROPERTY = ^D2D1_SUBPROPERTY;
  D2D1_SUBPROPERTY = Dword;
  {$EXTERNALSYM D2D1_SUBPROPERTY}
const
  D2D1_SUBPROPERTY_DISPLAYNAME = D2D1_PROPERTY(0);
  {$EXTERNALSYM D2D1_SUBPROPERTY_DISPLAYNAME}
  D2D1_SUBPROPERTY_ISREADONLY  = D2D1_PROPERTY(1);
  {$EXTERNALSYM D2D1_SUBPROPERTY_ISREADONLY}
  D2D1_SUBPROPERTY_MIN         = D2D1_PROPERTY(2);
  {$EXTERNALSYM D2D1_SUBPROPERTY_MIN}
  D2D1_SUBPROPERTY_MAX         = D2D1_PROPERTY(3);
  {$EXTERNALSYM D2D1_SUBPROPERTY_MAX}
  D2D1_SUBPROPERTY_DEFAULT     = D2D1_PROPERTY(4);
  {$EXTERNALSYM D2D1_SUBPROPERTY_DEFAULT}
  D2D1_SUBPROPERTY_FIELDS      = D2D1_PROPERTY(5);
  {$EXTERNALSYM D2D1_SUBPROPERTY_FIELDS}
  D2D1_SUBPROPERTY_INDEX       = D2D1_PROPERTY(6);
  {$EXTERNALSYM D2D1_SUBPROPERTY_INDEX}
  //D2D1_SUBPROPERTY_FORCE_DWORD = FORCEDWORD;

type
  // This specifies how colors are interpolated.
  PD2D1_COLOR_INTERPOLATION_MODE = ^D2D1_COLOR_INTERPOLATION_MODE;
  D2D1_COLOR_INTERPOLATION_MODE = DWord;
  {$EXTERNALSYM D2D1_COLOR_INTERPOLATION_MODE}
const
  // Colors will be interpolated in straight alpha space.
  D2D1_COLOR_INTERPOLATION_MODE_STRAIGHT      = D2D1_COLOR_INTERPOLATION_MODE(0);
  {$EXTERNALSYM D2D1_COLOR_INTERPOLATION_MODE_STRAIGHT}
  // Colors will be interpolated in premultiplied alpha space.
  D2D1_COLOR_INTERPOLATION_MODE_PREMULTIPLIED = D2D1_COLOR_INTERPOLATION_MODE(1);
  {$EXTERNALSYM D2D1_COLOR_INTERPOLATION_MODE_PREMULTIPLIED}
  //D2D1_COLOR_INTERPOLATION_MODE_FORCE_DWORD   = FORCEDWORD;

type
  // This specifies options that apply to the device context for its lifetime.
  PD2D1_DEVICE_CONTEXT_OPTIONS = ^D2D1_DEVICE_CONTEXT_OPTIONS;
  D2D1_DEVICE_CONTEXT_OPTIONS = DWord;
  {$EXTERNALSYM D2D1_DEVICE_CONTEXT_OPTIONS}
const
  D2D1_DEVICE_CONTEXT_OPTIONS_NONE                               = D2D1_DEVICE_CONTEXT_OPTIONS(0);
  // Geometry rendering will be performed on many threads in parallel); a single
  // thread is the default.
  D2D1_DEVICE_CONTEXT_OPTIONS_ENABLE_MULTITHREADED_OPTIMIZATIONS = D2D1_DEVICE_CONTEXT_OPTIONS(1);
  //D2D1_DEVICE_CONTEXT_OPTIONS_FORCE_DWORD                        = FORCEDWORD;

type
  // Defines when font resources should be subset during printing.
  PD2D1_PRINT_FONT_SUBSET_MODE = ^D2D1_PRINT_FONT_SUBSET_MODE;
  D2D1_PRINT_FONT_SUBSET_MODE = DWord;
  {$EXTERNALSYM D2D1_PRINT_FONT_SUBSET_MODE}
const
  // Subset for used glyphs, send and discard font resource after every five pages
  D2D1_PRINT_FONT_SUBSET_MODE_DEFAULT     = D2D1_PRINT_FONT_SUBSET_MODE(0);
  {$EXTERNALSYM D2D1_PRINT_FONT_SUBSET_MODE_DEFAULT}
  // Subset for used glyphs, send and discard font resource after each page
  D2D1_PRINT_FONT_SUBSET_MODE_EACHPAGE    = D2D1_PRINT_FONT_SUBSET_MODE(1);
  {$EXTERNALSYM D2D1_PRINT_FONT_SUBSET_MODE_EACHPAGE}
  // Do not subset, reuse font for all pages, send it after first page
  D2D1_PRINT_FONT_SUBSET_MODE_NONE        = D2D1_PRINT_FONT_SUBSET_MODE(2);
  {$EXTERNALSYM D2D1_PRINT_FONT_SUBSET_MODE_NONE}
  //D2D1_PRINT_FONT_SUBSET_MODE_FORCE_DWORD = FORCEDWORD;

type
  // Describes mapped memory from the ID2D1Bitmap1.Map API.
  PD2D1_MAPPED_RECT = ^D2D1_MAPPED_RECT;
  D2D1_MAPPED_RECT = record
    pitch: UINT32;
    bits: PByte;
  end;
  {$EXTERNALSYM D2D1_MAPPED_RECT}

  // All parameters related to pushing a layer.
  PD2D1_LAYER_PARAMETERS1 = ^D2D1_LAYER_PARAMETERS1;
  D2D1_LAYER_PARAMETERS1 = record
    contentBounds: D2D1_RECT_F;
    geometricMask: ID2D1Geometry;
    maskAntialiasMode: D2D1_ANTIALIAS_MODE;
    maskTransform: D2D1_MATRIX_3X2_F;
    opacity: Single;
    opacityBrush: ID2D1Brush;
    layerOptions: D2D1_LAYER_OPTIONS1;
  end;
  {$EXTERNALSYM D2D1_LAYER_PARAMETERS1}

  // Contains the position and color of a gradient stop.
  PD2D1_GRADIENT_STOP = ^D2D1_GRADIENT_STOP;
  D2D1_GRADIENT_STOP = record
    position: Single;
    color: D2D1_COLOR_F;
  end;
  {$EXTERNALSYM D2D1_GRADIENT_STOP}

  // Creation properties for an image brush.
  PD2D1_IMAGE_BRUSH_PROPERTIES = ^D2D1_IMAGE_BRUSH_PROPERTIES;
  D2D1_IMAGE_BRUSH_PROPERTIES = record
    sourceRectangle: D2D1_RECT_F;
    extendModeX: D2D1_EXTEND_MODE;
    extendModeY: D2D1_EXTEND_MODE;
    interpolationMode: D2D1_INTERPOLATION_MODE;
  end;
  {$EXTERNALSYM D2D1_IMAGE_BRUSH_PROPERTIES}


  // Describes the extend modes and the interpolation mode of an ID2D1BitmapBrush.
  PD2D1_BITMAP_BRUSH_PROPERTIES1 = ^D2D1_BITMAP_BRUSH_PROPERTIES1;
  D2D1_BITMAP_BRUSH_PROPERTIES1 = record
    extendModeX: D2D1_EXTEND_MODE;
    extendModeY: D2D1_EXTEND_MODE;
    interpolationMode: D2D1_INTERPOLATION_MODE;
  end;
  {$EXTERNALSYM D2D1_BITMAP_BRUSH_PROPERTIES1}

  // This controls advanced settings of the Direct2D imaging pipeline.
  PD2D1_RENDERING_CONTROLS = ^D2D1_RENDERING_CONTROLS;
  D2D1_RENDERING_CONTROLS = record
    // The default buffer precision, used if the precision isn't otherwise specified.
    bufferPrecision: D2D1_BUFFER_PRECISION;
    // The size of allocated tiles used to render imaging effects.
    tileSize: D2D1_SIZE_U;
  end;
  {$EXTERNALSYM D2D1_RENDERING_CONTROLS}


  // The creation properties for a ID2D1PrintControl object.
  PD2D1_PRINT_CONTROL_PROPERTIES = ^D2D1_PRINT_CONTROL_PROPERTIES;
  D2D1_PRINT_CONTROL_PROPERTIES = record
    fontSubset: D2D1_PRINT_FONT_SUBSET_MODE;
    // DPI for rasterization of all unsupported D2D commands or options, defaults to
    // 150.0
    rasterDPI: Single;
    // Color space for vector graphics in XPS package
    colorSpace: D2D1_COLOR_SPACE;
  end;
  {$EXTERNALSYM D2D1_PRINT_CONTROL_PROPERTIES}

  PWICImageParameters = ^TWICImageParameters;
  TWICImageParameters = record
    PixelFormat: D2D1_PIXEL_FORMAT;
    DpiX: single;
    DpiY: single;
    Top: single;
    Left: single;
    PixelWidth: UINT32;
    PixelHeight: UINT32;
  end;

  // Interface ID2D1ColorContext
  // ===========================
  // Represents a color context that can be used with an ID2D1Bitmap1 object.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1ColorContext);'}
  {$EXTERNALSYM ID2D1ColorContext}
  ID2D1ColorContext = interface(ID2D1Resource)
  ['{1c4820bb-5771-4518-a581-2fe4dd0ec657}']

    // Retrieves the color space of the color context.
    function GetColorSpace(): D2D1_COLOR_SPACE; stdcall;

    // Retrieves the size of the color profile, in bytes.
    function GetProfileSize(): UINT32; stdcall;

    // Retrieves the color profile bytes.
    function GetProfile(out profile: PByte;
                        profileSize: UINT32): HResult; stdcall;

  end;
  IID_ID2D1ColorContext = ID2D1ColorContext;
  {$EXTERNALSYM IID_ID2D1ColorContext}


   // Interface ID2D1ColorContext1
  // ============================
  // Represents a color context to be used with the Color Management Effect.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1ColorContext1);'}
  {$EXTERNALSYM ID2D1ColorContext1}
  ID2D1ColorContext1 = interface(ID2D1ColorContext)
  ['{1ab42875-c57f-4be9-bd85-9cd78d6f55ee}']

    // Retrieves the color context type.
    function GetColorContextType(): D2D1_COLOR_CONTEXT_TYPE;

    // Retrieves the DXGI color space of this context. Returns DXGI_COLOR_SPACE_CUSTOM
    // when color context type is ICC.
    function GetDXGIColorSpace(): DXGI_COLOR_SPACE_TYPE; stdcall;

    // Retrieves a set simple color profile.
    function GetSimpleColorProfile(out simpleProfile: D2D1_SIMPLE_COLOR_PROFILE): HResult; stdcall;

  end;

  // Interface ID2D1GradientStopCollection1
  // ======================================
  // Represents an collection of gradient stops that can then be the source resource
  // for either a linear or radial gradient brush.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1GradientStopCollection1);'}
  {$EXTERNALSYM ID2D1GradientStopCollection1}
  ID2D1GradientStopCollection1 = interface(ID2D1GradientStopCollection)
  ['{ae1572f4-5dd0-4777-998b-9279472ae63b}']

    // Copies the gradient stops from the collection into the caller's memory. If this
    // object was created using ID2D1DeviceContext.CreateGradientStopCollection, this
    // method returns the same values as were specified in the creation method. If this
    // object was created using ID2D1RenderTarget.CreateGradientStopCollection, the
    // stops returned here will first be transformed into the gamma space specified by
    // the colorInterpolationGamma parameter.
    procedure GetGradientStops1(out gradientStops: PD2D1_GRADIENT_STOP;
                                gradientStopsCount: UINT32); stdcall;

    // Returns the color space in which interpolation occurs. If this object was
    // created using ID2D1RenderTarget.CreateGradientStopCollection, this method
    // returns the color space related to the color interpolation gamma.
    function GetPreInterpolationSpace(): D2D1_COLOR_SPACE; stdcall;

    // Returns the color space colors will be converted to after interpolation occurs.
    // If this object was created using
    // ID2D1RenderTarget.CreateGradientStopCollection, this method returns
    // D2D1_COLOR_SPACE_SRGB.
    function GetPostInterpolationSpace(): D2D1_COLOR_SPACE; stdcall;

    // Returns the buffer precision of this gradient. If this object was created using
    // ID2D1RenderTarget.CreateGradientStopCollection, this method returns
    // D2D1_BUFFER_PRECISION_8BPC_UNORM.
    function GetBufferPrecision(): D2D1_BUFFER_PRECISION; stdcall;

    // Returns the interpolation mode used to interpolate colors in the gradient.
    function GetColorInterpolationMode(): D2D1_COLOR_INTERPOLATION_MODE; stdcall;

  end;
  IID_ID2D1GradientStopCollection1 = ID2D1GradientStopCollection1;
  {$EXTERNALSYM IID_ID2D1GradientStopCollection1}


  // Extended bitmap properties.
  PD2D1_BITMAP_PROPERTIES1 = ^D2D1_BITMAP_PROPERTIES1;
  D2D1_BITMAP_PROPERTIES1 = record
    _pixelFormat: D2D1_PIXEL_FORMAT;
    dpiX: Single;
    dpiY: Single;
    // Specifies how the bitmap can be used.
    bitmapOptions: D2D1_BITMAP_OPTIONS;
    colorContext: ID2D1ColorContext;
  end;
  {$EXTERNALSYM D2D1_BITMAP_PROPERTIES1}

  // Describes a point along a path.
  PD2D1_POINT_DESCRIPTION = ^D2D1_POINT_DESCRIPTION;
  D2D1_POINT_DESCRIPTION = record
    point: D2D1_POINT_2F;
    unitTangentVector: D2D1_POINT_2F;
    endSegment: UINT32;
    endFigure: UINT32;
    lengthToEndSegment: Single;
  end;
  {$EXTERNALSYM D2D1_POINT_DESCRIPTION}


  // Interface ID2D1Image
  // ====================
  // Represents a producer of pixels that can fill an arbitrary 2D plane.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Image);'}
  {$EXTERNALSYM ID2D1Image}
  ID2D1Image = interface(ID2D1Resource)
  ['{65019f75-8da2-497c-b32c-dfa34e48ede6}']

  end;
  IID_ID2D1Image = ID2D1Image;
  {$EXTERNALSYM IID_ID2D1Image}

  // Interface ID2D1GdiMetafileSink
  // ==============================
  // User-implementable interface for introspecting on a metafile.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1GdiMetafileSink);'}
  {$EXTERNALSYM ID2D1GdiMetafileSink}
  ID2D1GdiMetafileSink = interface(IUnknown)
  ['{82237326-8111-4f7c-bcf4-b5c1175564fe}']

    // Callback for examining a metafile record.
    function ProcessRecord(recordType: DWORD;
                           recordData: Pointer;
                           recordDataSize: DWORD): HResult; stdcall;

  end;
  IID_ID2D1GdiMetafileSink = ID2D1GdiMetafileSink;
  {$EXTERNALSYM IID_ID2D1GdiMetafileSink}


  // Interface ID2D1GdiMetafile
  // ==========================
  // Interface encapsulating a GDI/GDI+ metafile.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1GdiMetafile);'}
  {$EXTERNALSYM ID2D1GdiMetafile}
  ID2D1GdiMetafile = interface(ID2D1Resource)
  ['{2f543dc3-cfc1-4211-864f-cfd91c6f3395}']

    // Play the metafile into a caller-supplied sink interface.
    function Stream(sink: ID2D1GdiMetafileSink): HResult; stdcall;


    // Gets the bounds of the metafile.
    function GetBounds(out bounds: D2D1_RECT_F): HResult; stdcall;

  end;
  IID_ID2D1GdiMetafile = ID2D1GdiMetafile;
  {$EXTERNALSYM IID_ID2D1GdiMetafile}

  // Interface ID2D1Bitmap1
  // ======================
  // Represents a bitmap that can be used as a surface for an ID2D1DeviceContext or
  // mapped into system memory, and can contain additional color context information.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Bitmap1);'}
  {$EXTERNALSYM ID2D1Bitmap1}
  ID2D1Bitmap1 = interface(ID2D1Bitmap)
  ['{a898a84c-3873-4588-b08b-ebbf978df041}']

    // Retrieves the color context information associated with the bitmap.
    procedure GetColorContext(out colorContext: ID2D1ColorContext); stdcall;

    // Retrieves the bitmap options used when creating the API.
    function GetOptions(): D2D1_BITMAP_OPTIONS; stdcall;

    // Retrieves the DXGI surface from the corresponding bitmap, if the bitmap was
    // created from a device derived from a D3D device.
    function GetSurface(out dxgiSurface: IDXGISurface): HResult; stdcall;

    // Maps the given bitmap into memory. The bitmap must have been created with the
    // D2D1_BITMAP_OPTIONS_CPU_READ flag.
    function Map(options: D2D1_MAP_OPTIONS;
                 out mappedRect: D2D1_MAPPED_RECT): HResult; stdcall;

    // Unmaps the given bitmap from memory.
    function Unmap(): HResult; stdcall;

  end;
  IID_ID2D1Bitmap1 = ID2D1Bitmap1;
  {$EXTERNALSYM IID_ID2D1Bitmap1}

  // Interface ID2D1CommandSink
  // ==========================
  // Caller-supplied implementation of an interface to receive the recorded command
  // list.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1CommandSink);'}
  {$EXTERNALSYM ID2D1CommandSink}
  ID2D1CommandSink = interface(IUnknown)
  ['{54d7898a-a061-40a7-bec7-e465bcba2c4f}']

    function BeginDraw(): HResult; stdcall;

    function EndDraw(): HResult; stdcall;

    function SetAntialiasMode(antialiasMode: D2D1_ANTIALIAS_MODE): HResult; stdcall;

    function SetTags(tag1: D2D1_TAG;
                     tag2: D2D1_TAG): HResult; stdcall;

    function SetTextAntialiasMode(textAntialiasMode: D2D1_TEXT_ANTIALIAS_MODE): HResult; stdcall;
    // The text rendering options to be applied to all subsequent text and glyph
    // drawing operations; IUnknown(Nil) to clear current text rendering options.
    function SetTextRenderingParams(textRenderingParams: IDWriteRenderingParams): HResult; stdcall;

    function SetTransform(transform: D2D1_MATRIX_3X2_F): HResult; stdcall;

    function SetPrimitiveBlend(primitiveBlend: D2D1_PRIMITIVE_BLEND): HResult; stdcall;

    function SetUnitMode(unitMode: D2D1_UNIT_MODE): HResult; stdcall;

    function Clear(color: D2D1_COLOR_F): HResult; stdcall;

    function DrawGlyphRun(baselineOrigin: D2D1_POINT_2F;
                          glyphRun: DWRITE_GLYPH_RUN;
                          glyphRunDescription: DWRITE_GLYPH_RUN_DESCRIPTION;
                          foregroundBrush: ID2D1Brush;
                          measuringMode: DWRITE_MEASURING_MODE): HResult; stdcall;

    function DrawLine(point0: D2D1_POINT_2F;
                      point1: D2D1_POINT_2F;
                      brush: ID2D1Brush;
                      strokeWidth: Single;
                      strokeStyle: ID2D1StrokeStyle): HResult; stdcall;

    function DrawGeometry(geometry: ID2D1Geometry;
                          brush: ID2D1Brush;
                          strokeWidth: Single;
                          strokeStyle: ID2D1StrokeStyle): HResult; stdcall;

    function DrawRectangle(rect: D2D1_RECT_F;
                           brush: ID2D1Brush;
                           strokeWidth: Single;
                           strokeStyle: ID2D1StrokeStyle): HResult; stdcall;

    function DrawBitmap(bitmap: ID2D1Bitmap;
                        destinationRectangle: D2D1_RECT_F;
                        opacity: Single;
                        interpolationMode: D2D1_INTERPOLATION_MODE;
                        sourceRectangle: D2D1_RECT_F;
                        perspectiveTransform: D2D1_MATRIX_4X4_F): HResult; stdcall;

    function DrawImage(image: ID2D1Image;
                       targetOffset: D2D1_POINT_2F;
                       imageRectangle: D2D1_RECT_F;
                       interpolationMode: D2D1_INTERPOLATION_MODE;
                       compositeMode: D2D1_COMPOSITE_MODE): HResult; stdcall;

    function DrawGdiMetafile(gdiMetafile: ID2D1GdiMetafile;
                             targetOffset: D2D1_POINT_2F): HResult; stdcall;

    function FillMesh(mesh: ID2D1Mesh;
                      brush: ID2D1Brush): HResult; stdcall;

    function FillOpacityMask(opacityMask: ID2D1Bitmap;
                             brush: ID2D1Brush;
                             destinationRectangle: D2D1_RECT_F;
                             sourceRectangle: D2D1_RECT_F): HResult; stdcall;

    function FillGeometry(geometry: ID2D1Geometry;
                          brush: ID2D1Brush;
                          opacityBrush: ID2D1Brush): HResult; stdcall;

    function FillRectangle(rect: D2D1_RECT_F;
                           brush: ID2D1Brush): HResult; stdcall;

    function PushAxisAlignedClip(clipRect: D2D1_RECT_F;
                                 antialiasMode: D2D1_ANTIALIAS_MODE): HResult; stdcall;

    function PushLayer(layerParameters1: D2D1_LAYER_PARAMETERS1;
                       layer: ID2D1Layer): HResult; stdcall;

    function PopAxisAlignedClip(): HResult; stdcall;

    function PopLayer(): HResult; stdcall;

  end;
  IID_ID2D1CommandSink = ID2D1CommandSink;
  {$EXTERNALSYM IID_ID2D1CommandSink}

  // Interface ID2D1CommandList
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1CommandList);'}
  {$EXTERNALSYM ID2D1CommandList}
  ID2D1CommandList = interface(ID2D1Image)
  ['{b4f34a19-2383-4d76-94f6-ec343657c3dc}']

    // Play the command list into a caller-supplied sink interface.
    function Stream(sink: ID2D1CommandSink): HResult; stdcall;

    // Marks the command list as ready for use.
    function Close(): HResult; stdcall;

  end;
  IID_ID2D1CommandList = ID2D1CommandList;
  {$EXTERNALSYM IID_ID2D1CommandList}


  IID_ID2D1ColorContext1 = ID2D1ColorContext1;
  {$EXTERNALSYM IID_ID2D1ColorContext1}

  // Interface ID2D1SvgGlyphStyle
  // ============================
  // This object supplies the values for context-fill, context-stroke, and
  // context-value that are used when rendering SVG glyphs.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1SvgGlyphStyle);'}
  {$EXTERNALSYM ID2D1SvgGlyphStyle}
  ID2D1SvgGlyphStyle = interface(ID2D1Resource)
  ['{af671749-d241-4db8-8e41-dcc2e5c1a438}']

    // Provides values to an SVG glyph for fill. The brush with opacity set to 1 is
    // used as the 'context-fill'. The opacity of the brush is used as the
    // 'context-fill-opacity' value.

    // <param name="brush">A null brush will cause the context-fill value to come from
    // the defaultFillBrush. If the defaultFillBrush is also null, the context-fill
    // value will be 'none'.</param>
    function SetFill({in_opt} brush: ID2D1Brush): HResult; stdcall;

    // Returns the requested fill parameters.
    procedure GetFill(out brush: ID2D1Brush); stdcall;

    // Provides values to an SVG glyph for stroke properties. The brush with opacity
    // set to 1 is used as the 'context-stroke'. The opacity of the brush is used as
    // the 'context-stroke-opacity' value.

    // <param name="brush">A null brush will cause the context-stroke value to be
    // 'none'.</param>
    // <param name="strokeWidth">Specifies the 'context-value' for the 'stroke-width'
    // property.</param>
    // <param name="dashes">Specifies the 'context-value' for the 'stroke-dasharray'
    // property. A null value will cause the stroke-dasharray to be set to 'none'.
    // </param>
    // <param name="dashOffset">Specifies the 'context-value' for the
    // 'stroke-dashoffset' property.</param>
    function SetStroke({in_opt} brush: ID2D1Brush;
                       strokeWidth: Single = 1.0;
                       dashes: Single = 0.0;
                       dashesCount: UINT32 = 0;
                       dashOffset: Single = 1.0): HResult; stdcall;

    // Returns the number of dashes in the dash array.
    function GetStrokeDashesCount(): UINT32;

    // Returns the requested stroke parameters.
    procedure GetStroke(out brush: ID2D1Brush;
                        {out} strokeWidth: Single = 0.0;
                        {out} dashes: Single = 0.0;
                        dashesCount: UINT32 = 0;
                        {out} dashOffset: Single = 0.0); stdcall;

  end;
  IID_ID2D1SvgGlyphStyle = ID2D1SvgGlyphStyle;
  {$EXTERNALSYM IID_ID2D1SvgGlyphStyle}

  // Represents a point, radius pair that makes up part of a D2D1_INK_BEZIER_SEGMENT.
  PD2D1_INK_POINT = ^D2D1_INK_POINT;
  D2D1_INK_POINT = record
    x: Single;
    y: Single;
    radius: Single;
  end;
  {$EXTERNALSYM D2D1_INK_POINT}

  // Properties of a transformed image source.
  PD2D1_TRANSFORMED_IMAGE_SOURCE_PROPERTIES = ^D2D1_TRANSFORMED_IMAGE_SOURCE_PROPERTIES;
  D2D1_TRANSFORMED_IMAGE_SOURCE_PROPERTIES = record
    // The orientation at which the image source is drawn.
    orientation: D2D1_ORIENTATION;
    // The horizontal scale factor at which the image source is drawn.
    scaleX: Single;
    // The Single scale factor at which the image source is drawn.
    scaleY: Single;
    // The interpolation mode used when the image source is drawn.  This is ignored if
    // the image source is drawn using the DrawImage method, or using an image brush.
    interpolationMode: D2D1_INTERPOLATION_MODE;
    // Option flags.
    options: D2D1_TRANSFORMED_IMAGE_SOURCE_OPTIONS;
  end;
  {$EXTERNALSYM D2D1_TRANSFORMED_IMAGE_SOURCE_PROPERTIES}


  // Represents a Bezier segment to be used in the creation of an ID2D1Ink object.
  // This structure differs from D2D1_BEZIER_SEGMENT in that it is composed of
  // D2D1_INK_POINT s, which contain a radius in addition to x- and y-coordinates.
  PD2D1_INK_BEZIER_SEGMENT = ^D2D1_INK_BEZIER_SEGMENT;
  D2D1_INK_BEZIER_SEGMENT = record
    point1: D2D1_INK_POINT;
    point2: D2D1_INK_POINT;
    point3: D2D1_INK_POINT;
  end;
  {$EXTERNALSYM D2D1_INK_BEZIER_SEGMENT}


type
  // Specifies the appearance of the ink nib (pen tip) as part of an
  // D2D1_INK_STYLE_PROPERTIES structure.
  PD2D1_INK_NIB_SHAPE = ^D2D1_INK_NIB_SHAPE;
  D2D1_INK_NIB_SHAPE = DWord;
  {$EXTERNALSYM D2D1_INK_NIB_SHAPE}
const
  D2D1_INK_NIB_SHAPE_ROUND     = D2D1_INK_NIB_SHAPE(0);
  {$EXTERNALSYM D2D1_INK_NIB_SHAPE_ROUND}
  D2D1_INK_NIB_SHAPE_SQUARE    = D2D1_INK_NIB_SHAPE(1);
  {$EXTERNALSYM D2D1_INK_NIB_SHAPE_SQUARE}
  //D2D1_INK_NIB_SHAPE_FORCE_DWORD = FORCEDWORD;

type
  // Defines the general pen tip shape and the transform used in an ID2D1InkStyle
  // object.
  PD2D1_INK_STYLE_PROPERTIES = ^D2D1_INK_STYLE_PROPERTIES;
  D2D1_INK_STYLE_PROPERTIES = record
    // The general shape of the nib used to draw a given ink object.
    nibShape: D2D1_INK_NIB_SHAPE;
    // The transform applied to shape of the nib. _31 and _32 are ignored.
    nibTransform: D2D1_MATRIX_3X2_F;
  end;
  {$EXTERNALSYM D2D1_INK_STYLE_PROPERTIES}

  // Interface ID2D1InkStyle
  // =======================
  // Represents a collection of style properties to be used by methods like
  // ID2D1DeviceContext2.DrawInk when rendering ink. The ink style defines the nib
  // (pen tip) shape and transform.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1InkStyle);'}
  {$EXTERNALSYM ID2D1InkStyle}
  ID2D1InkStyle = interface(ID2D1Resource)
  ['{bae8b344-23fc-4071-8cb5-d05d6f073848}']

    procedure SetNibTransform(transform: D2D1_MATRIX_3X2_F); stdcall;

    procedure GetNibTransform(out transform: D2D1_MATRIX_3X2_F); stdcall;

    procedure SetNibShape(nibShape: D2D1_INK_NIB_SHAPE); stdcall;

    function GetNibShape(): D2D1_INK_NIB_SHAPE;

  end;
  IID_ID2D1InkStyle = ID2D1InkStyle;
  {$EXTERNALSYM IID_ID2D1InkStyle}


  // Interface ID2D1Ink
  // ==================
  // Represents a single continuous stroke of variable-width ink, as defined by a
  // series of Bezier segments and widths.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Ink);'}
  {$EXTERNALSYM ID2D1Ink}
  ID2D1Ink = interface(ID2D1Resource)
  ['{b499923b-7029-478f-a8b3-432c7c5f5312}']

    // Resets the ink start point.
    procedure SetStartPoint(startPoint: D2D1_INK_POINT); stdcall;

    // Retrieve the start point with which the ink was initialized.
    function GetStartPoint(): D2D1_INK_POINT; stdcall;

    // Add one or more segments to the end of the ink.
    function AddSegments(segments: D2D1_INK_BEZIER_SEGMENT;
                         segmentsCount: UINT32): HResult; stdcall;

    // Remove one or more segments from the end of the ink.
    function RemoveSegmentsAtEnd(isegmentsCount: UINT32): HResult; stdcall;

    // Updates the specified segments with new control points.
    function SetSegments(startSegment: UINT32;
                         segments: PD2D1_INK_BEZIER_SEGMENT; // pointer to array of segments
                         segmentsCount: UINT32): HResult; stdcall;

    // Update the last segment with new control points.
    function SetSegmentAtEnd(segment: PD2D1_INK_BEZIER_SEGMENT): HResult; stdcall;

    // Returns the number of segments the ink is composed of.
    function GetSegmentCount(): UINT32;

    // Retrieve the segments stored in the ink.
    function GetSegments(startSegment: UINT32;
                         {out} segments: PD2D1_INK_BEZIER_SEGMENT;  // returns pointer to array of segments
                         {out} segmentsCount: UINT32): HResult; stdcall;

    // Construct a geometric representation of the ink.
    function StreamAsGeometry({in_opt} inkStyle: ID2D1InkStyle;
                              {in_opt} worldTransform: PD2D1MATRIX3X2F;
                              flatteningTolerance: Single;
                              geometrySink: ID2D1SimplifiedGeometrySink): HResult; stdcall;

    // Retrieve the bounds of the ink, with an optional applied transform.
    function GetBounds({in_opt} inkStyle: ID2D1InkStyle;
                       {in_opt} worldTransform: PD2D1MATRIX3X2F;
                       out bounds: D2D1_RECT_F): HResult; stdcall;

  end;
  IID_ID2D1Ink = ID2D1Ink;
  {$EXTERNALSYM IID_ID2D1Ink}

type
  // Specifies how to render gradient mesh edges.
  PD2D1_PATCH_EDGE_MODE = ^D2D1_PATCH_EDGE_MODE;
  D2D1_PATCH_EDGE_MODE = DWord;
  {$EXTERNALSYM D2D1_PATCH_EDGE_MODE}
const
  // Render this edge aliased.
  D2D1_PATCH_EDGE_MODE_ALIASED          = D2D1_PATCH_EDGE_MODE(0);
  {$EXTERNALSYM D2D1_PATCH_EDGE_MODE_ALIASED}
  // Render this edge antialiased.
  D2D1_PATCH_EDGE_MODE_ANTIALIASED      = D2D1_PATCH_EDGE_MODE(1);
  {$EXTERNALSYM D2D1_PATCH_EDGE_MODE_ANTIALIASED}
  // Render this edge aliased and inflated out slightly.
  D2D1_PATCH_EDGE_MODE_ALIASED_INFLATED = D2D1_PATCH_EDGE_MODE(2);
  {$EXTERNALSYM D2D1_PATCH_EDGE_MODE_ALIASED_INFLATED}
  //D2D1_PATCH_EDGE_MODE_FORCE_DWORD    = FORCEDWORD;

type
  // Represents a tensor patch with 16 control points, 4 corner colors, and boundary
  // flags. An ID2D1GradientMesh is made up of 1 or more gradient mesh patches. Use
  // the GradientMeshPatch function or the GradientMeshPatchFromCoonsPatch function
  // to create one.
  PD2D1_GRADIENT_MESH_PATCH = ^D2D1_GRADIENT_MESH_PATCH;
  D2D1_GRADIENT_MESH_PATCH = record
    // The gradient mesh patch control point at position 00.
    point00: D2D1_POINT_2F;
    // The gradient mesh patch control point at position 01.
    point01: D2D1_POINT_2F;
    // The gradient mesh patch control point at position 02.
    point02: D2D1_POINT_2F;
    // The gradient mesh patch control point at position 03.
    point03: D2D1_POINT_2F;
    // The gradient mesh patch control point at position 10.
    point10: D2D1_POINT_2F;
    // The gradient mesh patch control point at position 11.
    point11: D2D1_POINT_2F;
    // The gradient mesh patch control point at position 12.
    point12: D2D1_POINT_2F;
    // The gradient mesh patch control point at position 13.
    point13: D2D1_POINT_2F;
    // The gradient mesh patch control point at position 20.
    point20: D2D1_POINT_2F;
    // The gradient mesh patch control point at position 21.
    point21: D2D1_POINT_2F;
    // The gradient mesh patch control point at position 22.
    point22: D2D1_POINT_2F;
    // The gradient mesh patch control point at position 23.
    point23: D2D1_POINT_2F;
    // The gradient mesh patch control point at position 30.
    point30: D2D1_POINT_2F;
    // The gradient mesh patch control point at position 31.
    point31: D2D1_POINT_2F;
    // The gradient mesh patch control point at position 32.
    point32: D2D1_POINT_2F;
    // The gradient mesh patch control point at position 33.
    point33: D2D1_POINT_2F;
    // The color associated with control point at position 00.
    color00: D2D1_COLOR_F;
    // The color associated with control point at position 03.
    color03: D2D1_COLOR_F;
    // The color associated with control point at position 30.
    color30: D2D1_COLOR_F;
    // The color associated with control point at position 33.
    color33: D2D1_COLOR_F;
    // The edge mode for the top edge of the patch.
    topEdgeMode: D2D1_PATCH_EDGE_MODE;
    // The edge mode for the left edge of the patch.
    leftEdgeMode: D2D1_PATCH_EDGE_MODE;
    // The edge mode for the bottom edge of the patch.
    bottomEdgeMode: D2D1_PATCH_EDGE_MODE;
    // The edge mode for the right edge of the patch.
    rightEdgeMode: D2D1_PATCH_EDGE_MODE;
  end;
  {$EXTERNALSYM D2D1_GRADIENT_MESH_PATCH}

  // Interface ID2D1GradientMesh
  // ===========================
  // Represents a device-dependent representation of a gradient mesh composed of
  // patches. Use the ID2D1DeviceContext2::CreateGradientMesh method to create an
  // instance of ID2D1GradientMesh.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1GradientMesh);'}
  {$EXTERNALSYM ID2D1GradientMesh}
  ID2D1GradientMesh = interface(ID2D1Resource)
  ['{f292e401-c050-4cde-83d7-04962d3b23c2}']

    // Returns the number of patches of the gradient mesh.
    function GetPatchCount(): UINT32; stdcall;

    // Retrieve the patch data stored in the gradient mesh.
    function GetPatches(startIndex: UINT32;
                        out patches: PD2D1_GRADIENT_MESH_PATCH; // returns pointer to array of patches
                        patchesCount: UINT32): HResult; stdcall;

  end;
  IID_ID2D1GradientMesh = ID2D1GradientMesh;
  {$EXTERNALSYM IID_ID2D1GradientMesh}

  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1SpriteBatch);'}
  {$EXTERNALSYM ID2D1SpriteBatch}
  ID2D1SpriteBatch = interface(ID2D1Resource)
  ['{4dc583bf-3a10-438a-8722-e9765224f1f1}']

    // Adds sprites to the end of the sprite batch.
    function AddSprites(spriteCount: UINT32;
                        destinationRectangles: D2D1_RECT_F;  // pointer to array of rectangles
                        sourceRectangles: PD2D1RECTU = Nil; // pointer to array of rectangles
                        colors: PD2D1COLORF = Nil;          // pointer to array of colors
                        transforms: PD2D1MATRIX3X2F = Nil; // pointer to array of transforms
                        destinationRectanglesStride: UINT32 = SizeOf(D2D1_RECT_F);
                        sourceRectanglesStride: UINT32 = SizeOf(D2D1_RECT_U);
                        colorsStride: UINT32 = SizeOf(D2D1_COLOR_F);
                        transformsStride: UINT32 = SizeOf(D2D1_MATRIX_3X2_F)): HResult; stdcall;

    // Set properties for existing sprites. All properties not specified are
    // unmodified.
    function SetSprites(startIndex: UINT32;
                        spriteCount: UINT32;
                        destinationRectangles: PD2D1RECTF = Nil;
                        sourceRectangles: PD2D1RECTU = Nil;
                        colors: PD2D1COLORF = Nil;
                        transforms: PD2D1MATRIX3X2F = Nil;
                        destinationRectanglesStride: UINT32 = SizeOf(D2D1_RECT_F);
                        sourceRectanglesStride: UINT32 = SizeOf(D2D1_RECT_U);
                        colorsStride: UINT32 = SizeOf(D2D1_COLOR_F);
                        transformsStride: UINT32 = SizeOf(D2D1_MATRIX_3X2_F)): HResult; stdcall;

    // Retrieves sprite properties.
    function GetSprites(startIndex: UINT32;
                        spriteCount: UINT32;
                        {out} destinationRectangles: PD2D1RECTF = Nil;
                        {out} sourceRectangles: PD2D1RECTU = Nil;
                        {out} colors: PD2D1COLORF = Nil;
                        {out} transforms: PD2D1MATRIX3X2F = Nil): HResult; stdcall;

    // Retrieves the number of sprites in the sprite batch.
    function GetSpriteCount(): UINT32;

    // Removes all sprites from the sprite batch.
    procedure Clear(); stdcall;

  end;

  // Interface ID2D1ImageSource
  // ==========================
  // Represents a producer of pixels that can fill an arbitrary 2D plane.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1ImageSource);'}
  {$EXTERNALSYM ID2D1ImageSource}
  ID2D1ImageSource = interface(ID2D1Image)
  ['{c9b664e5-74a1-4378-9ac2-eefc37a3f4d8}']

    function OfferResources(): HResult; stdcall;

    function TryReclaimResources(out resourcesDiscarded: BOOL): HResult; stdcall;

  end;
  IID_ID2D1ImageSource = ID2D1ImageSource;
  {$EXTERNALSYM IID_ID2D1ImageSource}


  // Interface ID2D1ImageSourceFromWic
  // =================================
  // Produces 2D pixel data that has been sourced from WIC.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1ImageSourceFromWic);'}
  {$EXTERNALSYM ID2D1ImageSourceFromWic}
  ID2D1ImageSourceFromWic = interface(ID2D1ImageSource)
  ['{77395441-1c8f-4555-8683-f50dab0fe792}']

    function EnsureCached({in_opt} rectangleToFill: PD2D1RECTU): HResult; stdcall;

    function TrimCache({in_opt} rectangleToPreserve: PD2D1RECTU): HResult; stdcall;

    procedure GetSource(out wicBitmapSource: IWICBitmapSource); stdcall;

  end;
  IID_ID2D1ImageSourceFromWic = ID2D1ImageSourceFromWic;
  {$EXTERNALSYM IID_ID2D1ImageSourceFromWic}

  // Interface ID2D1TransformedImageSource
  // =====================================
  // Represents an image source which shares resources with an original image source.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1TransformedImageSource);'}
  {$EXTERNALSYM ID2D1TransformedImageSource}
  ID2D1TransformedImageSource = interface(ID2D1Image)
  ['{7f1f79e5-2796-416c-8f55-700f911445e5}']

    procedure GetSource(out imageSource: ID2D1ImageSource); stdcall;

    procedure GetProperties(out properties: D2D1_TRANSFORMED_IMAGE_SOURCE_PROPERTIES); stdcall;

  end;
  IID_ID2D1TransformedImageSource = ID2D1TransformedImageSource;
  {$EXTERNALSYM IID_ID2D1TransformedImageSource}


  // Interface ID2D1ImageBrush
  // =========================
  // Provides a brush that can take any effect, command list or bitmap and use it to
  // fill a 2D shape.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1ImageBrush);'}
  {$EXTERNALSYM ID2D1ImageBrush}
  ID2D1ImageBrush = interface(ID2D1Brush)
  ['{fe9e984d-3f95-407c-b5db-cb94d4e8f87c}']

    procedure SetImage(image: ID2D1Image); stdcall;

    procedure SetExtendModeX(extendModeX: D2D1_EXTEND_MODE); stdcall;

    procedure SetExtendModeY(extendModeY: D2D1_EXTEND_MODE); stdcall;

    procedure SetInterpolationMode(interpolationMode: D2D1_INTERPOLATION_MODE); stdcall;

    procedure SetSourceRectangle(sourceRectangle: D2D1_RECT_F); stdcall;

    procedure GetImage(out image: ID2D1Image); stdcall;

    function GetExtendModeX(): D2D1_EXTEND_MODE; stdcall;

    function GetExtendModeY(): D2D1_EXTEND_MODE; stdcall;

    function GetInterpolationMode(): D2D1_INTERPOLATION_MODE; stdcall;

    procedure GetSourceRectangle(out sourceRectangle: D2D1_RECT_F); stdcall;

  end;
  IID_ID2D1ImageBrush = ID2D1ImageBrush;
  {$EXTERNALSYM IID_ID2D1ImageBrush}


  // Interface ID2D1BitmapBrush1
  // ===========================
  // A bitmap brush allows a bitmap to be used to fill a geometry.  Interpolation
  // mode is specified with D2D1_INTERPOLATION_MODE
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1BitmapBrush1);'}
  {$EXTERNALSYM ID2D1BitmapBrush1}
  ID2D1BitmapBrush1 = interface(ID2D1BitmapBrush)
  ['{41343a53-e41a-49a2-91cd-21793bbb62e5}']

    // Sets the interpolation mode used when this brush is used.
    procedure SetInterpolationMode1(interpolationMode: D2D1_INTERPOLATION_MODE); stdcall;

    function GetInterpolationMode1(): D2D1_INTERPOLATION_MODE; stdcall;

  end;
  IID_ID2D1BitmapBrush1 = ID2D1BitmapBrush1;
  {$EXTERNALSYM IID_ID2D1BitmapBrush1}

  // Interface ID2D1LookupTable3D
  // ============================
  // A container for 3D lookup table data that can be passed to the LookupTable3D
  // effect.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1LookupTable3D);'}
  {$EXTERNALSYM ID2D1LookupTable3D}
  ID2D1LookupTable3D = interface(ID2D1Resource)
  ['{53dd9855-a3b0-4d5b-82e1-26e25c5e5797}']

  end;
  IID_ID2D1LookupTable3D = ID2D1LookupTable3D;
  {$EXTERNALSYM IID_ID2D1LookupTable3D}


  IID_ID2D1SpriteBatch = ID2D1SpriteBatch;
  {$EXTERNALSYM IID_ID2D1SpriteBatch}

  // Interface ID2D1GeometryRealization
  // ==================================
  // Encapsulates a device- and transform-dependent representation of a filled or
  // stroked geometry.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1GeometryRealization);'}
  {$EXTERNALSYM ID2D1GeometryRealization}
  ID2D1GeometryRealization = interface(ID2D1Resource)
  ['{a16907d7-bc02-4801-99e8-8cf7f485f774}']

  end;
  IID_ID2D1GeometryRealization = ID2D1GeometryRealization;
  {$EXTERNALSYM IID_ID2D1GeometryRealization}


  // Interface ID2D1Properties
  // =========================
  // Represents a set of run-time bindable and discoverable properties that allow a
  // data-driven application to modify the state of a Direct2D effect.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Properties);'}
  {$EXTERNALSYM ID2D1Properties}
  ID2D1Properties = interface(IUnknown)
  ['{483473d7-cd46-4f9d-9d3a-3112aa80159d}']

    // Returns the total number of custom properties in this interface.
    function GetPropertyCount(): UINT32; stdcall;

    // Retrieves the property name from the given property index.
    function GetPropertyName(index: UINT32;
                             out name: LPWSTR;
                             nameCount: UINT32): HResult; stdcall;

    // Returns the length of the property name from the given index.
    function GetPropertyNameLength(index: UINT32): UINT32; stdcall;

    // Retrieves the type of the given property.
    function GetType(index: UINT32): D2D1_PROPERTY_TYPE; stdcall;

    // Retrieves the property index for the given property name.
    function GetPropertyIndex(name: LPWSTR): UINT32; stdcall;


    // Sets the value of the given property using its name.
    function SetValueByName(name: LPWSTR;
                            _type: D2D1_PROPERTY_TYPE;
                            data: PByte;
                            dataSize: UINT32): HResult; stdcall;

    // Sets the given value using the property index.
    function SetValue(index: UINT32;
                      _type: D2D1_PROPERTY_TYPE;
                      data: PByte;
                      dataSize: UINT32): HResult; stdcall;

    // Retrieves the given property or sub-property by name. '.' is the delimiter for
    // sub-properties.
    function GetValueByName(name: LPWSTR;
                            _type: D2D1_PROPERTY_TYPE;
                            data: PByte;
                            dataSize: UINT32): HResult; stdcall;


    // Retrieves the given value by index.
    function GetValue(index: UINT32;
                      _type: D2D1_PROPERTY_TYPE;
                      data: PByte;
                      dataSize: UINT32): HResult; stdcall;


    // Returns the value size for the given property index.
    function GetValueSize(index: UINT32): UINT32; stdcall;


    // Retrieves the sub-properties of the given property by index.
    function GetSubProperties(index: UINT32;
                              out subProperties: ID2D1Properties): HResult; stdcall;

  end;
  IID_ID2D1Properties = ID2D1Properties;
  {$EXTERNALSYM IID_ID2D1Properties}


  // Interface ID2D1Effect
  // =====================
  // The effect interface. Properties control how the effect is rendered. The effect
  // is Drawn with the DrawImage call.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Effect);'}
  {$EXTERNALSYM ID2D1Effect}
  ID2D1Effect = interface(ID2D1Properties)
  ['{28211a43-7d89-476f-8181-2d6159b220ad}']

    // Sets the input to the given effect. The input can be a concrete bitmap or the
    // output of another effect.
    procedure SetInput(index: UINT32;
                       input: ID2D1Image = Nil;
                       invalidate: BOOL = TRUE); stdcall;

    // If the effect supports a variable number of inputs, this sets the number of
    // input that are currently active on the effect.
    function SetInputCount(inputCount: UINT32): HResult; stdcall;

    // Returns the input image to the effect. The input could be another effect or a
    // bitmap.
    procedure GetInput(index: UINT32;
                       out input: ID2D1Image); stdcall;

    // This returns the number of input that are bound into this effect.
    function GetInputCount(): UINT32; stdcall;


    // Returns the output image of the given effect. This can be set as the input to
    // another effect or can be drawn with DrawImage.
    procedure GetOutput(out outputImage: ID2D1Image); stdcall;

  end;
  IID_ID2D1Effect = ID2D1Effect;
  {$EXTERNALSYM IID_ID2D1Effect}

  // This identifies a certain input connection of a certain effect.
  PD2D1_EFFECT_INPUT_DESCRIPTION = ^D2D1_EFFECT_INPUT_DESCRIPTION;
  D2D1_EFFECT_INPUT_DESCRIPTION = record
    // The effect whose input connection is being specified.
    effect: ID2D1Effect;
    // The index of the input connection into the specified effect.
    inputIndex: UINT32;
    // The rectangle which would be available on the specified input connection during
    // render operations.
    inputRectangle: D2D1_RECT_F;
  end;
  {$EXTERNALSYM D2D1_EFFECT_INPUT_DESCRIPTION}


  // Interface IPrintDocumentPackageTarget
  // =====================================
  // Document Target IPrintDocumentPackageTarget interface:
  // Allows user to enumerate supported package target types and create one with type ID.
  // It also supports tracking package printing progess and cancelling.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPrintDocumentPackageTarget);'}
  {$EXTERNALSYM IPrintDocumentPackageTarget}
  IPrintDocumentPackageTarget = interface(IUnknown)
  ['{1b8efec4-3019-4c27-964e-367202156906}']

    // This method is called for enumerating supported target types.
    // The first GUID is preferred type by target.
    function GetPackageTargetTypes(out targetCount: UINT32;
                                   out targetTypes: PGUID): HResult; stdcall;

    // This method is called for createing a target instance.")]
    function GetPackageTarget({in} const guidTargetType: TGUID;
                              {in} const riid: TGUID;
                              out ppvTarget: Pointer): HResult; stdcall;

    function Cancel(): HResult; stdcall;
  end;
  IID_IPrintDocumentPackageTarget = IPrintDocumentPackageTarget;
  {$EXTERNALSYM IID_IPrintDocumentPackageTarget}


  PPrintDocumentPackageCompletion = ^PrintDocumentPackageCompletion;
  PrintDocumentPackageCompletion              = (
    PrintDocumentPackageCompletion_InProgress	= 0,
    PrintDocumentPackageCompletion_Completed	= ( PrintDocumentPackageCompletion_InProgress + 1),
    PrintDocumentPackageCompletion_Canceled	= ( PrintDocumentPackageCompletion_Completed + 1),
    PrintDocumentPackageCompletion_Failed	= ( PrintDocumentPackageCompletion_Canceled + 1)
  );
  {$EXTERNALSYM PrintDocumentPackageCompletion}


  PPrintDocumentPackageStatus = ^PrintDocumentPackageStatus;
  PrintDocumentPackageStatus = record
    JobId: UINT32;
    CurrentDocument: INT32;
    CurrentPage: INT32;
    CurrentPageTotal: INT32;
    Completion: PrintDocumentPackageCompletion;
    PackageStatus: HResult;
  end;
  {$EXTERNALSYM PrintDocumentPackageStatus}


  // Interface ID2D1PrintControl
  // ===========================
  // Converts Direct2D primitives stored in an ID2D1CommandList into a fixed page
  // representation. The print sub-system then consumes the primitives.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1PrintControl);'}
  {$EXTERNALSYM ID2D1PrintControl}
  ID2D1PrintControl = interface(IUnknown)
  ['{2c1d867d-c290-41c8-ae7e-34a98702e9a5}']

    function AddPage(commandList: ID2D1CommandList;
                     pageSize: D2D_SIZE_F;
                     pagePrintTicketStream: IStream;
                     {out_opt} tag1: PD2D1TAG = Nil;
                     {out_opt} tag2: PD2D1TAG = Nil): HResult; stdcall;

    function Close(): HResult; stdcall;

  end;
  IID_ID2D1PrintControl = ID2D1PrintControl;
  {$EXTERNALSYM IID_ID2D1PrintControl}


  PID2D1Device = ^ID2D1Device;
  ID2D1Device = interface;

  // Interface ID2D1DeviceContext
  // ============================
  // The device context represents a set of state and a command buffer that is used
  // to render to a target bitmap.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1DeviceContext);'}
  {$EXTERNALSYM ID2D1DeviceContext}
  ID2D1DeviceContext = interface(ID2D1RenderTarget)
  ['{e8f7fe7a-191c-466d-ad95-975678bda998}']

    // Creates a bitmap with extended bitmap properties, potentially from a block of
    // memory.
    function CreateBitmap(size: D2D1_SIZE_U;
                          sourceData: Pointer;
                          pitch: UINT32;
                          bitmapProperties: PD2D1_BITMAP_PROPERTIES1;
                          out bitmap: ID2D1Bitmap1): HResult; stdcall;

    // Create a D2D bitmap by copying a WIC bitmap.
    function CreateBitmapFromWicBitmap(wicBitmapSource: IWICBitmapSource;
                                       bitmapProperties: PD2D1_BITMAP_PROPERTIES1;
                                       out bitmap: ID2D1Bitmap1): HResult; stdcall;

    // Creates a color context from a color space.  If the space is Custom, the context
    // is initialized from the profile/profileSize arguments.  Otherwise the context is
    // initialized with the profile bytes associated with the space and
    // profile/profileSize are ignored.
    function CreateColorContext(space: D2D1_COLOR_SPACE;
                                profile: PByte;
                                profileSize: UINT32;
                                out colorContext: ID2D1ColorContext): HResult; stdcall;

    function CreateColorContextFromFilename(filename: LPWSTR;
                                            out colorContext: ID2D1ColorContext): HResult; stdcall;

    function CreateColorContextFromWicColorContext(wicColorContext: IWICColorContext;
                                                   out colorContext: ID2D1ColorContext): HResult; stdcall;

    // Creates a bitmap from a DXGI surface with a set of extended properties.
    function CreateBitmapFromDxgiSurface(surface: IDXGISurface;
                                         {opt} bitmapProperties: PD2D1_BITMAP_PROPERTIES1;
                                         out bitmap: ID2D1Bitmap1): HResult; stdcall;

    // Create a new effect, the effect must either be built in or previously registered
    // through ID2D1Factory1.RegisterEffectFromStream or
    // ID2D1Factory1.RegisterEffectFromString.
    function CreateEffect(const effectId: TGUID;
                          out effect: ID2D1Effect): HResult; stdcall;

    // A gradient stop collection represents a set of stops in an ideal unit length.
    // This is the source resource for a linear gradient and radial gradient brush.

    // <param name="preInterpolationSpace">Specifies both the input color space and the
    // space in which the color interpolation occurs.</param>
    // <param name="postInterpolationSpace">Specifies the color space colors will be
    // converted to after interpolation occurs.</param>
    // <param name="bufferPrecision">Specifies the precision in which the gradient
    // buffer will be held.</param>
    // <param name="extendMode">Specifies how the gradient will be extended outside of
    // the unit length.</param>
    // <param name="colorInterpolationMode">Determines if colors will be interpolated
    // in straight alpha or premultiplied alpha space.</param>
    function CreateGradientStopCollection(straightAlphaGradientStops: D2D1_GRADIENT_STOP;
                                          straightAlphaGradientStopsCount: UINT32;
                                          preInterpolationSpace: D2D1_COLOR_SPACE;
                                          postInterpolationSpace: D2D1_COLOR_SPACE;
                                          bufferPrecision: D2D1_BUFFER_PRECISION;
                                          extendMode: D2D1_EXTEND_MODE;
                                          colorInterpolationMode: D2D1_COLOR_INTERPOLATION_MODE;
                                          out gradientStopCollection1: ID2D1GradientStopCollection1): HResult; stdcall;

    // Creates an image brush, the input image can be any type of image, including a
    // bitmap, effect and a command list.
    function CreateImageBrush(image: ID2D1Image;
                              imageBrushProperties: D2D1_IMAGE_BRUSH_PROPERTIES;
                              brushProperties: D2D1_BRUSH_PROPERTIES;
                              out imageBrush: ID2D1ImageBrush): HResult; stdcall;

    function CreateBitmapBrush(bitmap: ID2D1Bitmap;
                               bitmapBrushProperties: PD2D1_BITMAP_BRUSH_PROPERTIES1;
                               brushProperties: D2D1_BRUSH_PROPERTIES;
                               out bitmapBrush: ID2D1BitmapBrush1): HResult; stdcall;

    // Creates a new command list.
    function CreateCommandList(out commandList: ID2D1CommandList): HResult; stdcall;

    // Indicates whether the format is supported by D2D.
    function IsDxgiFormatSupported(format: DXGI_FORMAT): BOOL; stdcall;

    // Indicates whether the buffer precision is supported by D2D.
    function IsBufferPrecisionSupported(bufferPrecision: D2D1_BUFFER_PRECISION): BOOL; stdcall;

    // This retrieves the local-space bounds in DIPs of the current image using the
    // device context DPI.
    function GetImageLocalBounds(image: ID2D1Image;
                                 out localBounds: D2D1_RECT_F): HResult; stdcall;

    // This retrieves the world-space bounds in DIPs of the current image using the
    // device context DPI.
    function GetImageWorldBounds(image: ID2D1Image;
                                 out worldBounds: D2D1_RECT_F): HResult; stdcall;

    // Retrieves the world-space bounds in DIPs of the glyph run using the device
    // context DPI.
    function GetGlyphRunWorldBounds(baselineOrigin: D2D1_POINT_2F;
                                    glyphRun: DWRITE_GLYPH_RUN;
                                    measuringMode: DWRITE_MEASURING_MODE;
                                    out bounds: D2D1_RECT_F): HResult; stdcall;

    // Retrieves the device associated with this device context.
    procedure GetDevice(out device: ID2D1Device); stdcall;

    // Sets the target for this device context to point to the given image. The image
    // can be a command list or a bitmap created with the D2D1_BITMAP_OPTIONS_TARGET
    // flag.
    procedure SetTarget(image: ID2D1Image); stdcall;

    // Gets the target that this device context is currently pointing to.
    procedure GetTarget(out image: ID2D1Image); stdcall;

    // Sets tuning parameters for internal rendering inside the device context.
    procedure SetRenderingControls(renderingControls: D2D1_RENDERING_CONTROLS); stdcall;

    // This retrieves the rendering controls currently selected into the device
    // context.
    procedure GetRenderingControls(out renderingControls: D2D1_RENDERING_CONTROLS); stdcall;

    // Changes the primitive blending mode for all of the rendering operations.
    procedure SetPrimitiveBlend(primitiveBlend: D2D1_PRIMITIVE_BLEND); stdcall;

    // Returns the primitive blend currently selected into the device context.
    function GetPrimitiveBlend(): D2D1_PRIMITIVE_BLEND; stdcall;

    // Changes the units used for all of the rendering operations.
    procedure SetUnitMode(unitMode: D2D1_UNIT_MODE); stdcall;

    // Returns the unit mode currently set on the device context.
    function GetUnitMode(): D2D1_UNIT_MODE; stdcall;

    // Draws the glyph run with an extended description to describe the glyphs.
    procedure DrawGlyphRun(baselineOrigin: D2D1_POINT_2F;
                           glyphRun: DWRITE_GLYPH_RUN;
                           glyphRunDescription: DWRITE_GLYPH_RUN_DESCRIPTION;
                           foregroundBrush: ID2D1Brush;
                           measuringMode: DWRITE_MEASURING_MODE = DWRITE_MEASURING_MODE_NATURAL); stdcall;

    // Draw an image to the device context. The image represents either a concrete
    // bitmap or the output of an effect graph.
    procedure DrawImage(image: ID2D1Image;
                        targetOffset: PD2D1POINT2F = Nil;
                        imageRectangle: PD2D1RECTF = Nil;
                        interpolationMode: D2D1_INTERPOLATION_MODE = D2D1_INTERPOLATION_MODE_LINEAR;
                        compositeMode: D2D1_COMPOSITE_MODE = D2D1_COMPOSITE_MODE_SOURCE_OVER); stdcall;

    // Draw a metafile to the device context.
    procedure DrawGdiMetafile(gdiMetafile: ID2D1GdiMetafile;
                              targetOffset: PD2D1POINT2F = Nil); stdcall;

    procedure DrawBitmap(bitmap: ID2D1Bitmap;
                         destinationRectangle: D2D1_RECT_F;
                         opacity: Single;
                         interpolationMode: D2D1_INTERPOLATION_MODE;
                         sourceRectangle: PD2D1RECTF = Nil;
                         perspectiveTransform: PD2D1_MATRIX_4X4_F = Nil); stdcall;

    // Push a layer on the device context.
    procedure PushLayer(layerParameters: D2D1_LAYER_PARAMETERS1;
                        layer: ID2D1Layer); stdcall;

    // This indicates that a portion of an effect's input is invalid. This method can
    // be called many times.
    function InvalidateEffectInputRectangle(effect: ID2D1Effect;
                                            input: UINT32;
                                            inputRectangle: D2D1_RECT_F): HResult; stdcall;

    // Gets the number of invalid ouptut rectangles that have accumulated at the
    // effect.
    function GetEffectInvalidRectangleCount(effect: ID2D1Effect;
                                            out rectangleCount: UINT32): HResult; stdcall;

    // Gets the invalid rectangles that are at the output of the effect.
    function GetEffectInvalidRectangles(effect: ID2D1Effect;
                                        out rectangles: PD2D1RECTF; // pointer to array of D2D1_RECT_F
                                        rectanglesCount: UINT32): HResult; stdcall;

    // Gets the maximum region of each specified input which would be used during a
    // subsequent rendering operation
    function GetEffectRequiredInputRectangles(renderEffect: ID2D1Effect;
                                              renderImageRectangle: D2D1_RECT_F;
                                              inputDescriptions: PD2D1_EFFECT_INPUT_DESCRIPTION;
                                              out requiredInputRects: D2D1_RECT_F; // pointer to array of D2D1_RECT_F
                                              inputCount: UINT32): HResult; stdcall;

    // Fill using the alpha channel of the supplied opacity mask bitmap. The brush
    // opacity will be modulated by the mask. The render target antialiasing mode must
    // be set to aliased.
    procedure FillOpacityMask(opacityMask: ID2D1Bitmap;
                              brush: ID2D1Brush;
                              destinationRectangle: PD2D1RECTF = Nil;
                              sourceRectangle: PD2D1RECTF = Nil); stdcall;

  end;
  IID_ID2D1DeviceContext = ID2D1DeviceContext;
  {$EXTERNALSYM IID_ID2D1DeviceContext}

   // Interface ID2D1Device
  // =====================
  // The device defines a resource domain whose objects and device contexts can be
  // used together.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Device);'}
  {$EXTERNALSYM ID2D1Device}
  ID2D1Device = interface(ID2D1Resource)
  ['{47dd575d-ac05-4cdd-8049-9b02cd16f44c}']

    // Creates a new device context with no initially assigned target.
    function CreateDeviceContext(options: D2D1_DEVICE_CONTEXT_OPTIONS;
                                 out deviceContext: ID2D1DeviceContext): HResult; stdcall;

    // Creates a D2D print control.
    function CreatePrintControl(wicFactory: IWICImagingFactory;
                                documentTarget: IPrintDocumentPackageTarget;
                                printControlProperties: D2D1_PRINT_CONTROL_PROPERTIES;
                                out printControl: ID2D1PrintControl): HResult; stdcall;

    // Sets the maximum amount of texture memory to maintain before evicting caches.
    procedure SetMaximumTextureMemory(maximumInBytes: UINT64); stdcall;

    // Gets the maximum amount of texture memory to maintain before evicting caches.
    function GetMaximumTextureMemory(): UINT64; stdcall;

    // Clears all resources that are cached but not held in use by the application
    // through an interface reference.
    procedure ClearResources(millisecondsSinceUse: UINT32 = 0); stdcall;

  end;
  IID_ID2D1Device = ID2D1Device;
  {$EXTERNALSYM IID_ID2D1Device}


  // Interface ID2D1DeviceContext1
  // =============================
  // Enables creation and drawing of geometry realization objects.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1DeviceContext1);'}
  {$EXTERNALSYM ID2D1DeviceContext1}
  ID2D1DeviceContext1 = interface(ID2D1DeviceContext)
  ['{d37f57e4-6908-459f-a199-e72f24f79987}']

    function CreateFilledGeometryRealization(geometry: ID2D1Geometry;
                                             flatteningTolerance: Single;
                                             out geometryRealization: ID2D1GeometryRealization): HResult; stdcall;

    function CreateStrokedGeometryRealization(geometry: ID2D1Geometry;
                                              flatteningTolerance: Single;
                                              strokeWidth: Single;
                                              {in_opt} strokeStyle: ID2D1StrokeStyle;
                                              out geometryRealization: ID2D1GeometryRealization): HResult; stdcall;

    procedure DrawGeometryRealization(geometryRealization: ID2D1GeometryRealization;
                                      brush: ID2D1Brush) stdcall;

  end;
  IID_ID2D1DeviceContext1 = ID2D1DeviceContext1;
  {$EXTERNALSYM IID_ID2D1DeviceContext1}


  // Interface ID2D1DeviceContext2
  // =============================
  // This interface performs all the same functions as the ID2D1DeviceContext1
  // interface, plus it enables functionality such as ink rendering, gradient mesh
  // rendering, and improved image loading.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1DeviceContext2);'}
  {$EXTERNALSYM ID2D1DeviceContext2}
  ID2D1DeviceContext2 = interface(ID2D1DeviceContext1)
  ['{394ea6a3-0c34-4321-950b-6ca20f0be6c7}']

    function CreateInk(startPoint: D2D1_INK_POINT;
                       out ink: ID2D1Ink): HResult; stdcall;

    // Creates a new ink style.
    function CreateInkStyle({in_opt} inkStyleProperties: PD2D1_INK_STYLE_PROPERTIES;
                            out inkStyle: ID2D1InkStyle): HResult; stdcall;

    function CreateGradientMesh(patches: D2D1_GRADIENT_MESH_PATCH;
                                patchesCount: UINT32;
                                out gradientMesh: ID2D1GradientMesh): HResult; stdcall;

    function CreateImageSourceFromWic(wicBitmapSource: IWICBitmapSource;
                                      loadingOptions: D2D1_IMAGE_SOURCE_LOADING_OPTIONS;
                                      alphaMode: D2D1_ALPHA_MODE;
                                      out imageSource: ID2D1ImageSourceFromWic): HResult; stdcall;

    // Creates a 3D lookup table for mapping a 3-channel input to a 3-channel output.
    // The table data must be provided in 4-channel format.
    function CreateLookupTable3D(precision: D2D1_BUFFER_PRECISION;
                                 extents: UINT32;
                                 data: PByte;
                                 dataCount: UINT32;
                                 strides: UINT32;
                                 out lookupTable: ID2D1LookupTable3D): HResult; stdcall;

    function CreateImageSourceFromDxgi(surfaces: IDXGISurface;
                                       surfaceCount: UINT32;
                                       colorSpace: DXGI_COLOR_SPACE_TYPE;
                                       options: D2D1_IMAGE_SOURCE_FROM_DXGI_OPTIONS;
                                       out imageSource: ID2D1ImageSource): HResult; stdcall;

    // Retrieves the world-space bounds in DIPs of the gradient mesh using the device
    // context DPI.
    function GetGradientMeshWorldBounds(gradientMesh: ID2D1GradientMesh;
                                        out pBounds: D2D1_RECT_F): HResult; stdcall;

    procedure DrawInk(ink: ID2D1Ink;
                      brush: ID2D1Brush;
                      {in_opt} inkStyle: ID2D1InkStyle); stdcall;

    procedure DrawGradientMesh(gradientMesh: ID2D1GradientMesh); stdcall;

    // Draw a metafile to the device context.
    procedure DrawGdiMetafile(gdiMetafile: ID2D1GdiMetafile;
                              {in_opt} destinationRectangle: PD2D1RECTF = Nil;
                              {in_opt} sourceRectangle: PD2D1RECTF = Nil); stdcall;

    // Creates an image source which shares resources with an original.
    function CreateTransformedImageSource(imageSource: ID2D1ImageSource;
                                          properties: D2D1_TRANSFORMED_IMAGE_SOURCE_PROPERTIES;
                                          out transformedImageSource: ID2D1TransformedImageSource): HResult; stdcall;

  end;

  IID_ID2D1DeviceContext2 = ID2D1DeviceContext2;
  {$EXTERNALSYM IID_ID2D1DeviceContext2}


  // Interface ID2D1DeviceContext3
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1DeviceContext3);'}
  {$EXTERNALSYM ID2D1DeviceContext3}
  ID2D1DeviceContext3 = interface(ID2D1DeviceContext2)
  ['{235a7496-8351-414c-bcd4-6672ab2d8e00}']

    // Creates a new sprite batch.
    function CreateSpriteBatch(out spriteBatch: ID2D1SpriteBatch): HResult; stdcall;

    // Draws sprites in a sprite batch.
    procedure DrawSpriteBatch(spriteBatch: ID2D1SpriteBatch;
                              startIndex: UINT32;
                              spriteCount: UINT32;
                              bitmap: ID2D1Bitmap;
                              interpolationMode: D2D1_BITMAP_INTERPOLATION_MODE = D2D1_BITMAP_INTERPOLATION_MODE_LINEAR;
                              spriteOptions: D2D1_SPRITE_OPTIONS = D2D1_SPRITE_OPTIONS_NONE); stdcall;

  end;
  IID_ID2D1DeviceContext3 = ID2D1DeviceContext3;
  {$EXTERNALSYM IID_ID2D1DeviceContext3}

   // Interface ID2D1DeviceContext4
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1DeviceContext4);'}
  {$EXTERNALSYM ID2D1DeviceContext4}
  ID2D1DeviceContext4 = interface(ID2D1DeviceContext3)
  ['{8c427831-3d90-4476-b647-c4fae349e4db}']

    // Creates an SVG glyph style object.
    function CreateSvgGlyphStyle(out svgGlyphStyle: ID2D1SvgGlyphStyle): HResult; stdcall;

    // Draws the text within the given layout rectangle. By default, this method
    // performs baseline snapping and renders color versions of glyphs in color fonts.

    // <param name="svgGlyphStyle">Object used to style SVG glyphs.</param>
    // <param name="colorPaletteIndex">The index used to select a color palette within
    // a color font.</param>
    procedure DrawText(_string: PWideChar;
                       stringLength: UINT32;
                       textFormat: IDWriteTextFormat;
                       layoutRect: D2D1_RECT_F;
                       {in_opt} defaultFillBrush: ID2D1Brush;
                       {in_opt} svgGlyphStyle: ID2D1SvgGlyphStyle;
                       colorPaletteIndex: UINT32 = 0;
                       options: D2D1_DRAW_TEXT_OPTIONS = D2D1_DRAW_TEXT_OPTIONS_ENABLE_COLOR_FONT;
                       measuringMode: DWRITE_MEASURING_MODE = DWRITE_MEASURING_MODE_NATURAL); stdcall;


    // Draw a text layout object. If the layout is not subsequently changed, this can
    // be more efficient than DrawText when drawing the same layout repeatedly.

    // <param name="svgGlyphStyle">Object used to style SVG glyphs.</param>
    // <param name="colorPaletteIndex">The index used to select a color palette within
    // a color font.</param>
    // <param name="options">The specified text options. If D2D1_DRAW_TEXT_OPTIONS_CLIP
    // is used, the text is clipped to the layout bounds. These bounds are derived from
    // the origin and the layout bounds of the corresponding IDWriteTextLayout object.
    // </param>
    procedure DrawTextLayout(origin: D2D1_POINT_2F;
                             textLayout: IDWriteTextLayout;
                             {in_opt} defaultFillBrush: ID2D1Brush;
                             {in_opt} svgGlyphStyle: ID2D1SvgGlyphStyle;
                             colorPaletteIndex: UINT32 = 0;
                             options: D2D1_DRAW_TEXT_OPTIONS = D2D1_DRAW_TEXT_OPTIONS_ENABLE_COLOR_FONT); stdcall;

    // Draws a color glyph run using one (and only one) of the bitmap formats-
    // DWRITE_GLYPH_IMAGE_FORMATS_PNG, DWRITE_GLYPH_IMAGE_FORMATS_JPEG,
    // DWRITE_GLYPH_IMAGE_FORMATS_TIFF, or
    // DWRITE_GLYPH_IMAGE_FORMATS_PREMULTIPLIED_B8G8R8A8.
    procedure DrawColorBitmapGlyphRun(glyphImageFormat: DWRITE_GLYPH_IMAGE_FORMATS;
                                      baselineOrigin: D2D1_POINT_2F;
                                      glyphRun: DWRITE_GLYPH_RUN;
                                      measuringMode: DWRITE_MEASURING_MODE = DWRITE_MEASURING_MODE_NATURAL;
                                      bitmapSnapOption: D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION = D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION_DEFAULT); stdcall;


    // Draws a color glyph run that has the format of DWRITE_GLYPH_IMAGE_FORMATS_SVG.
    // <param name="svgGlyphStyle">Object used to style SVG glyphs.</param>
    // <param name="colorPaletteIndex">The index used to select a color palette within
    // a color font. Note that this not the same as the paletteIndex in the
    // DWRITE_COLOR_GLYPH_RUN struct, which is not relevant for SVG glyphs.</param>
    procedure DrawSvgGlyphRun(baselineOrigin: D2D1_POINT_2F;
                              glyphRun: DWRITE_GLYPH_RUN;
                              {in_opt} defaultFillBrush: ID2D1Brush = Nil;
                              {in_opt} svgGlyphStyle: ID2D1SvgGlyphStyle = Nil;
                              colorPaletteIndex: UINT32 = 0;
                              measuringMode: DWRITE_MEASURING_MODE = DWRITE_MEASURING_MODE_NATURAL); stdcall;


    // Retrieves an image of the color bitmap glyph from the color glyph cache. If the
    // cache does not already contain the requested resource, it will be created. This
    // method may be used to extend the lifetime of a glyph image even after it is
    // evicted from the color glyph cache.

    // <param name="fontEmSize">The specified font size affects the choice of which
    // bitmap to use from the font. It also affects the output glyphTransform, causing
    // it to properly scale the glyph.</param>
    // <param name="glyphTransform">Output transform, which transforms from the glyph's
    // space to the same output space as the worldTransform. This includes the input
    // glyphOrigin, the glyph's offset from the glyphOrigin, and any other required
    // transformations.</param>
    function GetColorBitmapGlyphImage(glyphImageFormat: DWRITE_GLYPH_IMAGE_FORMATS;
                                      glyphOrigin: D2D1_POINT_2F;
                                      fontFace: IDWriteFontFace;
                                      fontEmSize: Single;
                                      glyphIndex: UINT16;
                                      isSideways: BOOL;
                                      {in_opt} worldTransform: PD2D1MATRIX3X2F;
                                      dpiX: Single;
                                      dpiY: Single;
                                      out glyphTransform: D2D1_MATRIX_3X2_F;
                                      out glyphImage: ID2D1Image): HResult; stdcall;


    // Retrieves an image of the SVG glyph from the color glyph cache. If the cache
    // does not already contain the requested resource, it will be created. This method
    // may be used to extend the lifetime of a glyph image even after it is evicted
    // from the color glyph cache.

    // <param name="fontEmSize">The specified font size affects the output
    // glyphTransform, causing it to properly scale the glyph.</param>
    // <param name="svgGlyphStyle">Object used to style SVG glyphs.</param>
    // <param name="colorPaletteIndex">The index used to select a color palette within
    // a color font. Note that this not the same as the paletteIndex in the
    // DWRITE_COLOR_GLYPH_RUN struct, which is not relevant for SVG glyphs.</param>
    // <param name="glyphTransform">Output transform, which transforms from the glyph's
    // space to the same output space as the worldTransform. This includes the input
    // glyphOrigin, the glyph's offset from the glyphOrigin, and any other required
    // transformations.</param>
    function GetSvgGlyphImage(glyphOrigin: D2D1_POINT_2F;
                              fontFace: IDWriteFontFace;
                              fontEmSize: Single;
                              glyphIndex: UINT16;
                              isSideways: BOOL;
                              {in_opt} worldTransform: PD2D1MATRIX3X2F;
                              {in_opt} defaultFillBrush: ID2D1Brush;
                              {in_opt} svgGlyphStyle: ID2D1SvgGlyphStyle;
                              colorPaletteIndex: UINT32;
                              out glyphTransform: D2D1_MATRIX_3X2_F;
                              out glyphImage: ID2D1CommandList): HResult; stdcall;

  end;
  IID_ID2D1DeviceContext4 = ID2D1DeviceContext4;
  {$EXTERNALSYM IID_ID2D1DeviceContext4}

  // Interface ID2D1PathGeometry1
  // ============================
  // The ID2D1PathGeometry1 interface adds functionality to ID2D1PathGeometry. In
  // particular, it provides the path geometry-specific
  // ComputePointAndSegmentAtLength method.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1PathGeometry1);'}
  {$EXTERNALSYM ID2D1PathGeometry1}
  ID2D1PathGeometry1 = interface(ID2D1PathGeometry)
  ['{62baa2d2-ab54-41b7-b872-787e0106a421}']

    function ComputePointAndSegmentAtLength(_length: Single;
                                            startSegment: UINT32;
                                            worldTransform: D2D1_MATRIX_3X2_F;
                                            flatteningTolerance: Single;
                                            out pointDescription: D2D1_POINT_DESCRIPTION): HResult; stdcall;

  end;
  IID_ID2D1PathGeometry1 = ID2D1PathGeometry1;
  {$EXTERNALSYM IID_ID2D1PathGeometry1}




{$Region ID2D1SvgDocument}

// Enums =======================================================================

type
  // Specifies the paint type for an SVG fill or stroke.
  PD2D1_SVG_PAINT_TYPE = ^D2D1_SVG_PAINT_TYPE;
  D2D1_SVG_PAINT_TYPE = DWord;
  {$EXTERNALSYM D2D1_SVG_PAINT_TYPE}
const
  // The fill or stroke is not rendered.
  D2D1_SVG_PAINT_TYPE_NONE        = D2D1_SVG_PAINT_TYPE(0);
  {$EXTERNALSYM D2D1_SVG_PAINT_TYPE_NONE}
  // A solid color is rendered.
  D2D1_SVG_PAINT_TYPE_COLOR       = D2D1_SVG_PAINT_TYPE(1);
  {$EXTERNALSYM D2D1_SVG_PAINT_TYPE_COLOR}
  // The current color is rendered.
  D2D1_SVG_PAINT_TYPE_CURRENT_COLOR   = D2D1_SVG_PAINT_TYPE(2);
  {$EXTERNALSYM D2D1_SVG_PAINT_TYPE_CURRENT_COLOR}
  // A paint server); defined by another element in the SVG document); is used.
  D2D1_SVG_PAINT_TYPE_URI         = D2D1_SVG_PAINT_TYPE(3);
  {$EXTERNALSYM D2D1_SVG_PAINT_TYPE_URI}
  // A paint server); defined by another element in the SVG document); is used. If the
  // paint server reference is invalid); fall back to D2D1_SVG_PAINT_TYPE_NONE.
  D2D1_SVG_PAINT_TYPE_URI_NONE      = D2D1_SVG_PAINT_TYPE(4);
  {$EXTERNALSYM D2D1_SVG_PAINT_TYPE_URI_NONE}
  // A paint server); defined by another element in the SVG document); is used. If the
  // paint server reference is invalid); fall back to D2D1_SVG_PAINT_TYPE_COLOR.
  D2D1_SVG_PAINT_TYPE_URI_COLOR     = D2D1_SVG_PAINT_TYPE(5);
  {$EXTERNALSYM D2D1_SVG_PAINT_TYPE_URI_COLOR}
  // A paint server); defined by another element in the SVG document); is used. If the
  // paint server reference is invalid); fall back to
  // D2D1_SVG_PAINT_TYPE_CURRENT_COLOR.
  D2D1_SVG_PAINT_TYPE_URI_CURRENT_COLOR = D2D1_SVG_PAINT_TYPE(6);
  {$EXTERNALSYM D2D1_SVG_PAINT_TYPE_URI_CURRENT_COLOR}
  //D2D1_SVG_PAINT_TYPE_FORCE_DWORD     = FORCEDWORD;

type
  // Specifies the units for an SVG length.
  PD2D1_SVG_LENGTH_UNITS = ^D2D1_SVG_LENGTH_UNITS;
  D2D1_SVG_LENGTH_UNITS = DWord;
  {$EXTERNALSYM D2D1_SVG_LENGTH_UNITS}
const
  // The length is unitless.
  D2D1_SVG_LENGTH_UNITS_NUMBER    = D2D1_SVG_LENGTH_UNITS(0);
  {$EXTERNALSYM D2D1_SVG_LENGTH_UNITS_NUMBER}
  // The length is a percentage value.
  D2D1_SVG_LENGTH_UNITS_PERCENTAGE  = D2D1_SVG_LENGTH_UNITS(1);
  {$EXTERNALSYM D2D1_SVG_LENGTH_UNITS_PERCENTAGE}
  //D2D1_SVG_LENGTH_UNITS_FORCE_DWORD = FORCEDWORD;

type
  // Specifies a value for the SVG display property.
  PD2D1_SVG_DISPLAY = ^D2D1_SVG_DISPLAY;
  D2D1_SVG_DISPLAY = DWord;
  {$EXTERNALSYM D2D1_SVG_DISPLAY}
const
  // The element uses the default display behavior.
  D2D1_SVG_DISPLAY_INLINE    = D2D1_SVG_DISPLAY(0);
  // The element and all children are not rendered directly.
  D2D1_SVG_DISPLAY_NONE    = D2D1_SVG_DISPLAY(1);
  //D2D1_SVG_DISPLAY_FORCE_DWORD = FORCEDWORD;

type
  // Specifies a value for the SVG visibility property.
  PD2D1_SVG_VISIBILITY = ^D2D1_SVG_VISIBILITY;
  D2D1_SVG_VISIBILITY = DWord;
  {$EXTERNALSYM D2D1_SVG_VISIBILITY}
const
  // The element is visible.
  D2D1_SVG_VISIBILITY_VISIBLE   = D2D1_SVG_VISIBILITY(0);
  {$EXTERNALSYM D2D1_SVG_VISIBILITY_VISIBLE}
  // The element is invisible.
  D2D1_SVG_VISIBILITY_HIDDEN    = D2D1_SVG_VISIBILITY(1);
  {$EXTERNALSYM D2D1_SVG_VISIBILITY_HIDDEN}
  //D2D1_SVG_VISIBILITY_FORCE_DWORD = FORCEDWORD;

type
  // Specifies a value for the SVG overflow property.
  PD2D1_SVG_OVERFLOW = ^D2D1_SVG_OVERFLOW;
  D2D1_SVG_OVERFLOW = DWord;
  {$EXTERNALSYM D2D1_SVG_OVERFLOW}
const
  // The element is not clipped to its viewport.
  D2D1_SVG_OVERFLOW_VISIBLE   = D2D1_SVG_OVERFLOW(0);
  {$EXTERNALSYM D2D1_SVG_OVERFLOW_VISIBLE}
  // The element is clipped to its viewport.
  D2D1_SVG_OVERFLOW_HIDDEN    = D2D1_SVG_OVERFLOW(1);
  {$EXTERNALSYM D2D1_SVG_OVERFLOW_HIDDEN}
  //D2D1_SVG_OVERFLOW_FORCE_DWORD = FORCEDWORD;

type
  // Specifies a value for the SVG stroke-linecap property.
  PD2D1_SVG_LINE_CAP = ^D2D1_SVG_LINE_CAP;
  D2D1_SVG_LINE_CAP = DWord;
  {$EXTERNALSYM D2D1_SVG_LINE_CAP}
const
  // The property is set to SVG's 'butt' value.
  D2D1_SVG_LINE_CAP_BUTT    = D2D1_CAP_STYLE_FLAT;
  {$EXTERNALSYM D2D1_SVG_LINE_CAP_BUTT}
  // The property is set to SVG's 'square' value.
  D2D1_SVG_LINE_CAP_SQUARE   = D2D1_CAP_STYLE_SQUARE;
  {$EXTERNALSYM D2D1_SVG_LINE_CAP_SQUARE}
  // The property is set to SVG's 'round' value.
  D2D1_SVG_LINE_CAP_ROUND    = D2D1_CAP_STYLE_ROUND;
  {$EXTERNALSYM D2D1_SVG_LINE_CAP_ROUND}
  //D2D1_SVG_LINE_CAP_FORCE_DWORD = FORCEDWORD;

type
  // Specifies a value for the SVG stroke-linejoin property.
  PD2D1_SVG_LINE_JOIN = ^D2D1_SVG_LINE_JOIN;
  D2D1_SVG_LINE_JOIN = DWord;
  {$EXTERNALSYM D2D1_SVG_LINE_JOIN}
const
  // The property is set to SVG's 'bevel' value.
  D2D1_SVG_LINE_JOIN_BEVEL     = D2D1_LINE_JOIN_BEVEL;
  {$EXTERNALSYM D2D1_SVG_LINE_JOIN_BEVEL}
  // The property is set to SVG's 'miter' value. Note that this is equivalent to
  // D2D1_LINE_JOIN_MITER_OR_BEVEL); not D2D1_LINE_JOIN_MITER.
  D2D1_SVG_LINE_JOIN_MITER     = D2D1_LINE_JOIN_MITER_OR_BEVEL;
  {$EXTERNALSYM D2D1_SVG_LINE_JOIN_MITER}
  // The property is set to SVG's 'round' value.
  D2D1_SVG_LINE_JOIN_ROUND     = D2D1_LINE_JOIN_ROUND;
  {$EXTERNALSYM D2D1_SVG_LINE_JOIN_ROUND}
  //D2D1_SVG_LINE_JOIN_FORCE_DWORD = FORCEDWORD;

type
  // The alignment portion of the SVG preserveAspectRatio attribute.
  PD2D1_SVG_ASPECT_ALIGN = ^D2D1_SVG_ASPECT_ALIGN;
  D2D1_SVG_ASPECT_ALIGN = DWord;
  {$EXTERNALSYM D2D1_SVG_ASPECT_ALIGN}
const
  // The alignment is set to SVG's 'none' value.
  D2D1_SVG_ASPECT_ALIGN_NONE    = D2D1_SVG_ASPECT_ALIGN(0);
  {$EXTERNALSYM D2D1_SVG_ASPECT_ALIGN_NONE}
  // The alignment is set to SVG's 'xMinYMin' value.
  D2D1_SVG_ASPECT_ALIGN_X_MIN_Y_MIN = D2D1_SVG_ASPECT_ALIGN(1);
  {$EXTERNALSYM D2D1_SVG_ASPECT_ALIGN_X_MIN_Y_MIN}
  // The alignment is set to SVG's 'xMidYMin' value.
  D2D1_SVG_ASPECT_ALIGN_X_MID_Y_MIN = D2D1_SVG_ASPECT_ALIGN(2);
  {$EXTERNALSYM D2D1_SVG_ASPECT_ALIGN_X_MID_Y_MIN}
  // The alignment is set to SVG's 'xMaxYMin' value.
  D2D1_SVG_ASPECT_ALIGN_X_MAX_Y_MIN = D2D1_SVG_ASPECT_ALIGN(3);
  {$EXTERNALSYM D2D1_SVG_ASPECT_ALIGN_X_MAX_Y_MIN}
  // The alignment is set to SVG's 'xMinYMid' value.
  D2D1_SVG_ASPECT_ALIGN_X_MIN_Y_MID = D2D1_SVG_ASPECT_ALIGN(4);
  {$EXTERNALSYM D2D1_SVG_ASPECT_ALIGN_X_MIN_Y_MID}
  // The alignment is set to SVG's 'xMidYMid' value.
  D2D1_SVG_ASPECT_ALIGN_X_MID_Y_MID = D2D1_SVG_ASPECT_ALIGN(5);
  {$EXTERNALSYM D2D1_SVG_ASPECT_ALIGN_X_MID_Y_MID}
  // The alignment is set to SVG's 'xMaxYMid' value.
  D2D1_SVG_ASPECT_ALIGN_X_MAX_Y_MID = D2D1_SVG_ASPECT_ALIGN(6);
  {$EXTERNALSYM D2D1_SVG_ASPECT_ALIGN_X_MAX_Y_MID}
  // The alignment is set to SVG's 'xMinYMax' value.
  D2D1_SVG_ASPECT_ALIGN_X_MIN_Y_MAX = D2D1_SVG_ASPECT_ALIGN(7);
  {$EXTERNALSYM D2D1_SVG_ASPECT_ALIGN_X_MIN_Y_MAX}
  // The alignment is set to SVG's 'xMidYMax' value.
  D2D1_SVG_ASPECT_ALIGN_X_MID_Y_MAX = D2D1_SVG_ASPECT_ALIGN(8);
  {$EXTERNALSYM D2D1_SVG_ASPECT_ALIGN_X_MID_Y_MAX}
  // The alignment is set to SVG's 'xMaxYMax' value.
  D2D1_SVG_ASPECT_ALIGN_X_MAX_Y_MAX = D2D1_SVG_ASPECT_ALIGN(9);
  {$EXTERNALSYM D2D1_SVG_ASPECT_ALIGN_X_MAX_Y_MAX}
  //D2D1_SVG_ASPECT_ALIGN_FORCE_DWORD = FORCEDWORD;

type
  // The meetOrSlice portion of the SVG preserveAspectRatio attribute.
  PD2D1_SVG_ASPECT_SCALING = ^D2D1_SVG_ASPECT_SCALING;
  D2D1_SVG_ASPECT_SCALING = DWord;
  {$EXTERNALSYM D2D1_SVG_ASPECT_SCALING}
const
  // Scale the viewBox up as much as possible such that the entire viewBox is visible
  // within the viewport.
  D2D1_SVG_ASPECT_SCALING_MEET    = D2D1_SVG_ASPECT_SCALING(0);
  {$EXTERNALSYM D2D1_SVG_ASPECT_SCALING_MEET}
  // Scale the viewBox down as much as possible such that the entire viewport is
  // covered by the viewBox.
  D2D1_SVG_ASPECT_SCALING_SLICE     = D2D1_SVG_ASPECT_SCALING(1);
  {$EXTERNALSYM D2D1_SVG_ASPECT_SCALING_SLICE}
  //D2D1_SVG_ASPECT_SCALING_FORCE_DWORD = FORCEDWORD;

type
  // Represents a path commmand. Each command may reference floats from the segment
  // data. Commands ending in _ABSOLUTE interpret data as absolute coordinate.
  // Commands ending in _RELATIVE interpret data as being relative to the previous
  // point.
  PD2D1_SVG_PATH_COMMAND = ^D2D1_SVG_PATH_COMMAND;
  D2D1_SVG_PATH_COMMAND = DWord;
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND}
const
  // Closes the current subpath. Uses no segment data.
  D2D1_SVG_PATH_COMMAND_CLOSE_PATH        = D2D1_SVG_PATH_COMMAND(0);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_CLOSE_PATH}
  // Starts a new subpath at the coordinate (x y). Uses 2 floats of segment data.
  D2D1_SVG_PATH_COMMAND_MOVE_ABSOLUTE       = D2D1_SVG_PATH_COMMAND(1);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_MOVE_ABSOLUTE}
  // Starts a new subpath at the coordinate (x y). Uses 2 floats of segment data.
  D2D1_SVG_PATH_COMMAND_MOVE_RELATIVE       = D2D1_SVG_PATH_COMMAND(2);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_MOVE_RELATIVE}
  // Draws a line to the coordinate (x y). Uses 2 floats of segment data.
  D2D1_SVG_PATH_COMMAND_LINE_ABSOLUTE       = D2D1_SVG_PATH_COMMAND(3);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_LINE_ABSOLUTE}
  // Draws a line to the coordinate (x y). Uses 2 floats of segment data.
  D2D1_SVG_PATH_COMMAND_LINE_RELATIVE       = D2D1_SVG_PATH_COMMAND(4);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_LINE_RELATIVE}
  // Draws a cubic Bezier curve (x1 y1 x2 y2 x y). The curve ends at (x); y) and is
  // defined by the two control points (x1); y1) and (x2); y2). Uses 6 floats of
  // segment data.
  D2D1_SVG_PATH_COMMAND_CUBIC_ABSOLUTE      = D2D1_SVG_PATH_COMMAND(5);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_CUBIC_ABSOLUTE}
  // Draws a cubic Bezier curve (x1 y1 x2 y2 x y). The curve ends at (x); y) and is
  // defined by the two control points (x1); y1) and (x2); y2). Uses 6 floats of
  // segment data.
  D2D1_SVG_PATH_COMMAND_CUBIC_RELATIVE      = D2D1_SVG_PATH_COMMAND(6);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_CUBIC_RELATIVE}
  // Draws a quadratic Bezier curve (x1 y1 x y). The curve ends at (x); y) and is
  // defined by the control point (x1 y1). Uses 4 floats of segment data.
  D2D1_SVG_PATH_COMMAND_QUADRADIC_ABSOLUTE    = D2D1_SVG_PATH_COMMAND(7);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_QUADRADIC_ABSOLUTE}
  // Draws a quadratic Bezier curve (x1 y1 x y). The curve ends at (x); y) and is
  // defined by the control point (x1 y1). Uses 4 floats of segment data.
  D2D1_SVG_PATH_COMMAND_QUADRADIC_RELATIVE    = D2D1_SVG_PATH_COMMAND(8);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_QUADRADIC_RELATIVE}
  // Draws an elliptical arc (rx ry x-axis-rotation large-arc-flag sweep-flag x y).
  // The curve ends at (x); y) and is defined by the arc parameters. The two flags are
  // considered set if their values are non-zero. Uses 7 floats of segment data.
  D2D1_SVG_PATH_COMMAND_ARC_ABSOLUTE        = D2D1_SVG_PATH_COMMAND(9);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_ARC_ABSOLUTE}
  // Draws an elliptical arc (rx ry x-axis-rotation large-arc-flag sweep-flag x y).
  // The curve ends at (x); y) and is defined by the arc parameters. The two flags are
  // considered set if their values are non-zero. Uses 7 floats of segment data.
  D2D1_SVG_PATH_COMMAND_ARC_RELATIVE        = D2D1_SVG_PATH_COMMAND(10);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_ARC_RELATIVE}
  // Draws a horizontal line to the coordinate (x). Uses 1 float of segment data.
  D2D1_SVG_PATH_COMMAND_HORIZONTAL_ABSOLUTE     = D2D1_SVG_PATH_COMMAND(11);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_HORIZONTAL_ABSOLUTE}
  // Draws a horizontal line to the coordinate (x). Uses 1 float of segment data.
  D2D1_SVG_PATH_COMMAND_HORIZONTAL_RELATIVE     = D2D1_SVG_PATH_COMMAND(12);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_HORIZONTAL_RELATIVE}
  // Draws a vertical line to the coordinate (y). Uses 1 float of segment data.
  D2D1_SVG_PATH_COMMAND_VERTICAL_ABSOLUTE     = D2D1_SVG_PATH_COMMAND(13);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_VERTICAL_ABSOLUTE}
  // Draws a vertical line to the coordinate (y). Uses 1 float of segment data.
  D2D1_SVG_PATH_COMMAND_VERTICAL_RELATIVE     = D2D1_SVG_PATH_COMMAND(14);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_VERTICAL_RELATIVE}
  // Draws a smooth cubic Bezier curve (x2 y2 x y). The curve ends at (x); y) and is
  // defined by the control point (x2); y2). Uses 4 floats of segment data.
  D2D1_SVG_PATH_COMMAND_CUBIC_SMOOTH_ABSOLUTE   = D2D1_SVG_PATH_COMMAND(15);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_CUBIC_SMOOTH_ABSOLUTE}
  // Draws a smooth cubic Bezier curve (x2 y2 x y). The curve ends at (x); y) and is
  // defined by the control point (x2); y2). Uses 4 floats of segment data.
  D2D1_SVG_PATH_COMMAND_CUBIC_SMOOTH_RELATIVE   = D2D1_SVG_PATH_COMMAND(16);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_CUBIC_SMOOTH_RELATIVE}
  // Draws a smooth quadratic Bezier curve ending at (x); y). Uses 2 floats of segment
  // data.
  D2D1_SVG_PATH_COMMAND_QUADRADIC_SMOOTH_ABSOLUTE = D2D1_SVG_PATH_COMMAND(17);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_QUADRADIC_SMOOTH_ABSOLUTE}
  // Draws a smooth quadratic Bezier curve ending at (x); y). Uses 2 floats of segment
  // data.
  D2D1_SVG_PATH_COMMAND_QUADRADIC_SMOOTH_RELATIVE = D2D1_SVG_PATH_COMMAND(18);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_QUADRADIC_SMOOTH_RELATIVE}
  //D2D1_SVG_PATH_COMMAND_FORCE_DWORD         = FORCEDWORD;

type
  // Defines the coordinate system used for SVG gradient or clipPath elements.
  PD2D1_SVG_UNIT_TYPE = ^D2D1_SVG_UNIT_TYPE;
  D2D1_SVG_UNIT_TYPE = DWord;
  {$EXTERNALSYM D2D1_SVG_UNIT_TYPE}
const
  // The property is set to SVG's 'userSpaceOnUse' value.
  D2D1_SVG_UNIT_TYPE_USER_SPACE_ON_USE   = D2D1_SVG_UNIT_TYPE(0);
  {$EXTERNALSYM D2D1_SVG_UNIT_TYPE_USER_SPACE_ON_USE}
  // The property is set to SVG's 'objectBoundingBox' value.
  D2D1_SVG_UNIT_TYPE_OBJECT_BOUNDING_BOX = D2D1_SVG_UNIT_TYPE(1);
  {$EXTERNALSYM D2D1_SVG_UNIT_TYPE_OBJECT_BOUNDING_BOX}
  //D2D1_SVG_UNIT_TYPE_FORCE_DWORD     = FORCEDWORD;

type
  // Defines the type of SVG string attribute to set or get.
  PD2D1_SVG_ATTRIBUTE_STRING_TYPE = ^D2D1_SVG_ATTRIBUTE_STRING_TYPE;
  D2D1_SVG_ATTRIBUTE_STRING_TYPE = DWord;
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_STRING_TYPE}
const
  // The attribute is a string in the same form as it would appear in the SVG XML.
  //
  // Note that when getting values of this type); the value returned may not exactly
  // match the value that was set. Instead); the output value is a normalized version
  // of the value. For example); an input color of 'red' may be output as '#FF0000'.
  D2D1_SVG_ATTRIBUTE_STRING_TYPE_SVG     = D2D1_SVG_ATTRIBUTE_STRING_TYPE(0);
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_STRING_TYPE_SVG}
  // The attribute is an element ID.
  D2D1_SVG_ATTRIBUTE_STRING_TYPE_ID      = D2D1_SVG_ATTRIBUTE_STRING_TYPE(1);
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_STRING_TYPE_ID}
  //D2D1_SVG_ATTRIBUTE_STRING_TYPE_FORCE_DWORD = FORCEDWORD;

type
  // Defines the type of SVG POD attribute to set or get.
  PD2D1_SVG_ATTRIBUTE_POD_TYPE = ^D2D1_SVG_ATTRIBUTE_POD_TYPE;
  D2D1_SVG_ATTRIBUTE_POD_TYPE = DWord;
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_POD_TYPE}
const
  // The attribute is a FLOAT.
  D2D1_SVG_ATTRIBUTE_POD_TYPE_FLOAT                = D2D1_SVG_ATTRIBUTE_POD_TYPE(0);
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_POD_TYPE_FLOAT}
  // The attribute is a D2D1_COLOR_F.
  D2D1_SVG_ATTRIBUTE_POD_TYPE_COLOR                = D2D1_SVG_ATTRIBUTE_POD_TYPE(1);
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_POD_TYPE_COLOR}
  // The attribute is a D2D1_FILL_MODE.
  D2D1_SVG_ATTRIBUTE_POD_TYPE_FILL_MODE            = D2D1_SVG_ATTRIBUTE_POD_TYPE(2);
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_POD_TYPE_FILL_MODE}
  // The attribute is a D2D1_SVG_DISPLAY.
  D2D1_SVG_ATTRIBUTE_POD_TYPE_DISPLAY              = D2D1_SVG_ATTRIBUTE_POD_TYPE(3);
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_POD_TYPE_DISPLAY}
  // The attribute is a D2D1_SVG_OVERFLOW.
  D2D1_SVG_ATTRIBUTE_POD_TYPE_OVERFLOW             = D2D1_SVG_ATTRIBUTE_POD_TYPE(4);
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_POD_TYPE_OVERFLOW}
  // The attribute is a D2D1_SVG_LINE_CAP.
  D2D1_SVG_ATTRIBUTE_POD_TYPE_LINE_CAP             = D2D1_SVG_ATTRIBUTE_POD_TYPE(5);
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_POD_TYPE_LINE_CAP}
  // The attribute is a D2D1_SVG_LINE_JOIN.
  D2D1_SVG_ATTRIBUTE_POD_TYPE_LINE_JOIN            = D2D1_SVG_ATTRIBUTE_POD_TYPE(6);
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_POD_TYPE_LINE_JOIN}
  // The attribute is a D2D1_SVG_VISIBILITY.
  D2D1_SVG_ATTRIBUTE_POD_TYPE_VISIBILITY           = D2D1_SVG_ATTRIBUTE_POD_TYPE(7);
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_POD_TYPE_VISIBILITY}
  // The attribute is a D2D1_MATRIX_3X2_F.
  D2D1_SVG_ATTRIBUTE_POD_TYPE_MATRIX               = D2D1_SVG_ATTRIBUTE_POD_TYPE(8);
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_POD_TYPE_MATRIX}
  // The attribute is a D2D1_SVG_UNIT_TYPE.
  D2D1_SVG_ATTRIBUTE_POD_TYPE_UNIT_TYPE             = D2D1_SVG_ATTRIBUTE_POD_TYPE(9);
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_POD_TYPE_UNIT_TYPE}
  // The attribute is a D2D1_EXTEND_MODE.
  D2D1_SVG_ATTRIBUTE_POD_TYPE_EXTEND_MODE           = D2D1_SVG_ATTRIBUTE_POD_TYPE(10);
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_POD_TYPE_EXTEND_MODE}
  // The attribute is a D2D1_SVG_PRESERVE_ASPECT_RATIO.
  D2D1_SVG_ATTRIBUTE_POD_TYPE_PRESERVE_ASPECT_RATIO = D2D1_SVG_ATTRIBUTE_POD_TYPE(11);
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_POD_TYPE_PRESERVE_ASPECT_RATIO}
  // The attribute is a D2D1_SVG_VIEWBOX.
  D2D1_SVG_ATTRIBUTE_POD_TYPE_VIEWBOX               = D2D1_SVG_ATTRIBUTE_POD_TYPE(12);
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_POD_TYPE_VIEWBOX}
  // The attribute is a D2D1_SVG_LENGTH.
  D2D1_SVG_ATTRIBUTE_POD_TYPE_LENGTH                = D2D1_SVG_ATTRIBUTE_POD_TYPE(13);
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_POD_TYPE_LENGTH}
  //D2D1_SVG_ATTRIBUTE_POD_TYPE_FORCE_DWORD         = FORCEDWORD;


// =============================================================================

type

  // Forward interface declarations
  ID2D1SvgAttribute = interface;
  PID2D1SvgAttribute = ^ID2D1SvgAttribute;

  ID2D1SvgPaint = interface;
  PID2D1SvgPaint = ^ID2D1SvgPaint;

  ID2D1SvgStrokeDashArray = interface;
  PID2D1SvgStrokeDashArray = ^ID2D1SvgStrokeDashArray;

  ID2D1SvgPointCollection = interface;
  PID2D1SvgPointCollection = ^ID2D1SvgPointCollection;

  ID2D1SvgPathData = interface;
  PID2D1SvgPathData = ^ID2D1SvgPathData;

  ID2D1SvgElement = interface;
  PID2D1SvgElement = ^ID2D1SvgElement;

  ID2D1SvgDocument = interface;
  PID2D1SvgDocument = ^ID2D1SvgDocument;



  // Represents an SVG length.
  PD2D1_SVG_LENGTH = ^D2D1_SVG_LENGTH;
  D2D1_SVG_LENGTH = record
    value: Single;
    units: D2D1_SVG_LENGTH_UNITS;
  end;
  {$EXTERNALSYM D2D1_SVG_LENGTH}


  // Represents all SVG preserveAspectRatio settings.
  PD2D1_SVG_PRESERVE_ASPECT_RATIO = ^D2D1_SVG_PRESERVE_ASPECT_RATIO;
  D2D1_SVG_PRESERVE_ASPECT_RATIO = record
    // Sets the 'defer' portion of the preserveAspectRatio settings. This field only
    // has an effect on an 'image' element that references another SVG document. As
    // this is not currently supported, the field has no impact on rendering.
    defer: BOOL;
    // Sets the align portion of the preserveAspectRatio settings.
    align: D2D1_SVG_ASPECT_ALIGN;
    // Sets the meetOrSlice portion of the preserveAspectRatio settings.
    meetOrSlice: D2D1_SVG_ASPECT_SCALING;
  end;
  {$EXTERNALSYM D2D1_SVG_PRESERVE_ASPECT_RATIO}


  // Represents an SVG viewBox.
  PD2D1_SVG_VIEWBOX = ^D2D1_SVG_VIEWBOX;
  D2D1_SVG_VIEWBOX = record
    x: Single;
    y: Single;
    width: Single;
    height: Single;
  end;
  {$EXTERNALSYM D2D1_SVG_VIEWBOX}


//#if NTDDI_VERSION >= NTDDI_WIN10_RS2


// INTERFACES //////////////////////////////////////////////////////////////////

  // Interface ID2D1SvgAttribute
  // ===========================
  // Interface describing an SVG attribute.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1SvgAttribute);'}
  {$EXTERNALSYM ID2D1SvgAttribute}
  ID2D1SvgAttribute = interface(ID2D1Resource)
  ['{c9cdb0dd-f8c9-4e70-b7c2-301c80292c5e}']

    // Returns the element on which this attribute is set. Returns null if the
    // attribute is not set on any element.
   procedure GetElement(out element: ID2D1SvgElement); stdcall;

    // Creates a clone of this attribute value. On creation, the cloned attribute is
    // not set on any element.
    function Clone(out attribute: ID2D1SvgAttribute): HResult; stdcall;

  end;
  IID_ID2D1SvgAttribute = ID2D1SvgAttribute;
  {$EXTERNALSYM IID_ID2D1SvgAttribute}


  // Interface ID2D1SvgPaint
  // =======================
  // Interface describing an SVG 'fill' or 'stroke' value.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1SvgPaint);'}
  {$EXTERNALSYM ID2D1SvgPaint}
  ID2D1SvgPaint = interface(ID2D1SvgAttribute)
  ['{d59bab0a-68a2-455b-a5dc-9eb2854e2490}']

    // Sets the paint type.
    function SetPaintType(paintType: D2D1_SVG_PAINT_TYPE): HResult; stdcall;

    // Gets the paint type.
    function GetPaintType(): D2D1_SVG_PAINT_TYPE; stdcall;

    // Sets the paint color that is used if the paint type is
    // D2D1_SVG_PAINT_TYPE_COLOR.
    function SetColor(Const color: D2D1_COLOR_F): HResult; stdcall;

    // Gets the paint color that is used if the paint type is
    // D2D1_SVG_PAINT_TYPE_COLOR.
    procedure GetColor(out color: D2D1_COLOR_F); stdcall;

    // Sets the element id which acts as the paint server. This id is used if the paint
    // type is D2D1_SVG_PAINT_TYPE_URI.
    function SetId(id: LPWSTR): HResult; stdcall;

    // Gets the element id which acts as the paint server. This id is used if the paint
    // type is D2D1_SVG_PAINT_TYPE_URI.
    function GetId(out id: PWideChar;
                   idCount: UINT32): HResult; stdcall;

    // Gets the string length of the element id which acts as the paint server. This id
    // is used if the paint type is D2D1_SVG_PAINT_TYPE_URI. The returned string length
    // does not include room for the null terminator.
    function GetIdLength(): UINT32; stdcall;

  end;
  IID_ID2D1SvgPaint = ID2D1SvgPaint;
  {$EXTERNALSYM IID_ID2D1SvgPaint}


  // Interface ID2D1SvgStrokeDashArray
  // =================================
  // Interface describing an SVG 'stroke-dasharray' value.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1SvgStrokeDashArray);'}
  {$EXTERNALSYM ID2D1SvgStrokeDashArray}
  ID2D1SvgStrokeDashArray = interface(ID2D1SvgAttribute)
  ['{f1c0ca52-92a3-4f00-b4ce-f35691efd9d9}']

    // Removes dashes from the end of the array.
    // <param name="dashesCount">Specifies how many dashes to remove.</param>
    function RemoveDashesAtEnd(dashesCount: UINT32): HResult; stdcall;

    // Updates the array. Existing dashes not updated by this method are preserved. The
    // array is resized larger if necessary to accomodate the new dashes.

    // <param name="dashes">The dashes array.</param>
    // <param name="dashesCount">The number of dashes to update.</param>
    // <param name="startIndex">The index at which to begin updating dashes. Must be
    // less than or equal to the size of the array.</param>
    function UpdateDashes(dashes: PSingle;
                          dashesCount: UINT32;
                          startIndex: UINT32 = 0): HResult; overload; stdcall;

    // Updates the array. Existing dashes not updated by this method are preserved. The
    // array is resized larger if necessary to accomodate the new dashes.

    // <param name="dashes">The dashes array.</param>
    // <param name="dashesCount">The number of dashes to update.</param>
    // <param name="startIndex">The index at which to begin updating dashes. Must be
    // less than or equal to the size of the array.</param>
    function UpdateDashes(dashes: PD2D1_SVG_LENGTH;
                          dashesCount: UINT32;
                          startIndex: UINT32 = 0): HResult; overload; stdcall;

    // Gets dashes from the array.

    // <param name="dashes">Buffer to contain the dashes.</param>
    // <param name="dashesCount">The element count of buffer.</param>
    // <param name="startIndex">The index of the first dash to retrieve.</param>
    function GetDashes(out dashes: PSingle;
                       dashesCount: UINT32;
                       startIndex: UINT32 = 0): HResult; overload; stdcall;

    // Gets dashes from the array.

    // <param name="dashes">Pointer to buffer to contain the dashes.</param>
    // <param name="dashesCount">The element count of buffer.</param>
    // <param name="startIndex">The index of the first dash to retrieve.</param>
    function GetDashes(out dashes: PD2D1_SVG_LENGTH;
                       dashesCount: UINT32;
                       startIndex: UINT32 = 0): HResult; overload; stdcall;

    // Gets the number of the dashes in the array.
    function GetDashesCount(): UINT32; stdcall;

  end;
  IID_ID2D1SvgStrokeDashArray = ID2D1SvgStrokeDashArray;
  {$EXTERNALSYM IID_ID2D1SvgStrokeDashArray}


  // Interface ID2D1SvgPointCollection
  // =================================
  // Interface describing an SVG 'points' value in a 'polyline' or 'polygon' element.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1SvgPointCollection);'}
  {$EXTERNALSYM ID2D1SvgPointCollection}
  ID2D1SvgPointCollection = interface(ID2D1SvgAttribute)
   ['{9dbe4c0d-3572-4dd9-9825-5530813bb712}']

    // Removes points from the end of the array.
    // <param name="pointsCount">Specifies how many points to remove.</param>
    function RemovePointsAtEnd(pointsCount: UINT32): HResult; stdcall;

    // Updates the points array. Existing points not updated by this method are
    // preserved. The array is resized larger if necessary to accomodate the new
    // points.

    // <param name="points">The points array.</param>
    // <param name="pointsCount">The number of points to update.</param>
    // <param name="startIndex">The index at which to begin updating points. Must be
    // less than or equal to the size of the array.</param>
    function UpdatePoints(points: PD2D1POINT2F;
                          pointsCount: UINT32;
                          startIndex: UINT32 = 0): HResult; stdcall;


    // Gets points from the points array.

    // <param name="points">Buffer to contain the points.</param>
    // <param name="pointsCount">The element count of the buffer.</param>
    // <param name="startIndex">The index of the first point to retrieve.</param>
    function GetPoints(out points: D2D1_POINT_2F;
                       pointsCount: UINT32;
                       startIndex: UINT32 = 0): HResult; stdcall;

    // Gets the number of points in the array.
    function GetPointsCount(): UINT32;

  end; // interface ID2D1SvgPointCollection
  IID_ID2D1SvgPointCollection = ID2D1SvgPointCollection;
  {$EXTERNALSYM IID_ID2D1SvgPointCollection}


  // Interface ID2D1SvgPathData
  // ==========================
  // Interface describing SVG path data. Path data can be set as the 'd' attribute on
  // a 'path' element.
  //
  // The path data set is factored into two arrays. The segment data array stores all
  // numbers and the commands array stores the set of commands. Unlike the string
  // data set in the d attribute, each command in this representation uses a fixed
  // number of elements in the segment data array. Therefore, the path 'M 0,0 100,0
  // 0,100 Z' is represented as: 'M0,0 L100,0 L0,100 Z'. This is split into two
  // arrays, with the segment data containing '0,0 100,0 0,100', and the commands
  // containing 'M L L Z'.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1SvgPathData);'}
  {$EXTERNALSYM ID2D1SvgPathData}
  ID2D1SvgPathData = interface(ID2D1SvgAttribute)
   ['{c095e4f4-bb98-43d6-9745-4d1b84ec9888}']

    // Removes data from the end of the segment data array.
    // <param name="dataCount">Specifies how much data to remove.</param>
    function RemoveSegmentDataAtEnd(dataCount: UINT32): HResult; stdcall;

    // Updates the segment data array. Existing segment data not updated by this method
    // are preserved. The array is resized larger if necessary to accomodate the new
    // segment data.

    // <param name="data">The data array.</param>
    // <param name="dataCount">The number of data to update.</param>
    // <param name="startIndex">The index at which to begin updating segment data. Must
    // be less than or equal to the size of the segment data array.</param>
    function UpdateSegmentData(data: PSingle;
                               dataCount: UINT32;
                               startIndex: UINT32 = 0): HResult; stdcall;

    // Gets data from the segment data array.
    // <param name="data">Buffer to contain the segment data array.</param>
    // <param name="dataCount">The element count of the buffer.</param>
    // <param name="startIndex">The index of the first segment data to retrieve.
    // </param>
    function GetSegmentData(out data: PSingle;
                            dataCount: UINT32;
                            startIndex: UINT32 = 0): HResult; stdcall;

    // Gets the size of the segment data array.
    function GetSegmentDataCount(): UINT32; stdcall;

    // Removes commands from the end of the commands array.
    // <param name="commandsCount">Specifies how many commands to remove.</param>
    function RemoveCommandsAtEnd(commandsCount: UINT32): HResult; stdcall;

    // Updates the commands array. Existing commands not updated by this method are
    // preserved. The array is resized larger if necessary to accomodate the new
    // commands.

    // <param name="commands">The commands array.</param>
    // <param name="commandsCount">The number of commands to update.</param>
    // <param name="startIndex">The index at which to begin updating commands. Must be
    // less than or equal to the size of the commands array.</param>
    function UpdateCommands(commands: PD2D1_SVG_PATH_COMMAND;
                            commandsCount: UINT32;
                            startIndex: UINT32 = 0): HResult; stdcall;

    // Gets commands from the commands array.

    // <param name="commands">Buffer to contain the commands</param>
    // <param name="commandsCount">The element count of the buffer.</param>
    // <param name="startIndex">The index of the first commands to retrieve.</param>
    function GetCommands(out commands: PD2D1_SVG_PATH_COMMAND;
                         commandsCount: UINT32;
                         startIndex: UINT32 = 0): HResult; stdcall;

    // Gets the size of the commands array.
    function GetCommandsCount(): UINT32; stdcall;

    // Creates a path geometry object representing the path data.
    function CreatePathGeometry(fillMode: D2D1_FILL_MODE;
                                out pathGeometry: ID2D1PathGeometry1): HResult; stdcall;

  end;
  IID_ID2D1SvgPathData = ID2D1SvgPathData;
  {$EXTERNALSYM IID_ID2D1SvgPathData}



  // Interface ID2D1SvgElement
  // =========================
  // Interface for all SVG elements.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1SvgElement);'}
  {$EXTERNALSYM ID2D1SvgElement}
  ID2D1SvgElement = interface(ID2D1Resource)
   ['{ac7b67a6-183e-49c1-a823-0ebe40b0db29}']

    // Gets the document that contains this element. Returns null if the element has
    // been removed from the tree.
    procedure GetDocument(document: ID2D1SvgDocument); stdcall;

    // Gets the tag name.
    function GetTagName(name: PWideChar;
                        nameCount: UINT32): HResult; stdcall;

    // Gets the string length of the tag name. The returned string length does not
    // include room for the null terminator.
    function GetTagNameLength(): UINT32; stdcall;

    // Returns TRUE if this element represents text content, e.g. the content of a
    // 'title' or 'desc' element. Text content does not have a tag name.
    function IsTextContent(): BOOL; stdcall;

    // Gets the parent element.
    procedure GetParent(out parent: ID2D1SvgElement); stdcall;

    // Returns whether this element has children.
    function HasChildren(): BOOL; stdcall;

    // Gets the first child of this element.
    procedure GetFirstChild(out child: ID2D1SvgElement); stdcall;

    // Gets the last child of this element.
    procedure GetLastChild(out child: ID2D1SvgElement); stdcall;

    // Gets the previous sibling of the referenceChild element.
    // <param name="referenceChild">The referenceChild must be an immediate child of
    // this element.</param>
    // <param name="previousChild">The output previousChild element will be non-nil if
    // the referenceChild has a previous sibling. If the referenceChild is the first
    // child, the output is nil.</param>
    function GetPreviousChild(const referenceChild: ID2D1SvgElement;
                              out previousChild: ID2D1SvgElement): HResult; stdcall;

    // Gets the next sibling of the referenceChild element.
    // <param name="referenceChild">The referenceChild must be an immediate child of
    // this element.</param>
    // <param name="nextChild">The output nextChild element will be non-nil if the
    // referenceChild has a next sibling. If the referenceChild is the last child, the
    // output is nil.</param>
    function GetNextChild(const referenceChild: ID2D1SvgElement;
                          out nextChild: ID2D1SvgElement): HResult; stdcall;

    // Inserts newChild as a child of this element, before the referenceChild element.
    // If the newChild element already has a parent, it is removed from this parent as
    // part of the insertion. Returns an error if this element cannot accept children
    // of the type of newChild. Returns an error if the newChild is an ancestor of this
    // element.
    // <param name="newChild">The element to be inserted.</param>
    // <param name="referenceChild">The element that the child should be inserted
    // before. If referenceChild is nil, the newChild is placed as the last child. If
    // referenceChild is non-nil, it must be an immediate child of this element.
    // </param>
    function InsertChildBefore(newChild: ID2D1SvgElement;
                               {In_opt} referenceChild: PID2D1SvgElement = Nil): HResult; stdcall;

    // Appends newChild to the list of children. If the newChild element already has a
    // parent, it is removed from this parent as part of the append operation. Returns
    // an error if this element cannot accept children of the type of newChild. Returns
    // an error if the newChild is an ancestor of this element.
    // <param name="newChild">The element to be appended.</param>
    function AppendChild(newChild: ID2D1SvgElement): HResult; stdcall;

    // Replaces the oldChild element with the newChild. This operation removes the
    // oldChild from the tree. If the newChild element already has a parent, it is
    // removed from this parent as part of the replace operation. Returns an error if
    // this element cannot accept children of the type of newChild. Returns an error if
    // the newChild is an ancestor of this element.
    // <param name="newChild">The element to be inserted.</param>
    // <param name="oldChild">The child element to be replaced. The oldChild element
    // must be an immediate child of this element.</param>
    function ReplaceChild(newChild: ID2D1SvgElement;
                          oldChild: ID2D1SvgElement): HResult; stdcall;

    // Removes the oldChild from the tree. Children of oldChild remain children of
    // oldChild.
    // <param name="oldChild">The child element to be removed. The oldChild element
    // must be an immediate child of this element.</param>
    function RemoveChild(oldChild: ID2D1SvgElement): HResult; stdcall;

    // Creates an element from a tag name. The element is appended to the list of
    // children. Returns an error if this element cannot accept children of the
    // specified type.
    // <param name="tagName">The tag name of the new child. An empty string is
    // interpreted to be a text content element.</param>
    // <param name="newChild">The new child element.</param>
    function CreateChild(tagName: LPWSTR;
                         out newChild: ID2D1SvgElement): HResult; stdcall;

    // Returns true if the attribute is explicitly set on the element or if it is
    // present within an inline style. Returns FALSE if the attribute is not a valid
    // attribute on this element.
    // <param name="name">The name of the attribute.</param>
    // <param name="inherited">Outputs whether the attribute is set to the 'inherit'
    // value.</param>
    function IsAttributeSpecified(name: LPWSTR;
                                  _inherited: PBOOL = Nil): BOOL; stdcall;

    // Returns the number of specified attributes on this element. Attributes are only
    // considered specified if they are explicitly set on the element or present within
    // an inline style. Properties that receive their value through CSS inheritance are
    // not considered specified. An attribute can become specified if it is set through
    // a method call. It can become unspecified if it is removed via RemoveAttribute.
    function GetSpecifiedAttributeCount(): UINT32; stdcall;

    // Gets the name of the specified attribute at the given index.
    // <param name="index">The specified index of the attribute.</param>
    // <param name="name">Outputs the name of the attribute.</param>
    // <param name="inherited">Outputs whether the attribute is set to the 'inherit'
    // value.</param>
    function GetSpecifiedAttributeName(index: UINT32;
                                       out name: PWideChar;
                                       nameCount: UINT32;
                                       _inherited: PBOOL = Nil): HResult; stdcall;

    // Gets the string length of the name of the specified attribute at the given
    // index. The output string length does not include room for the null terminator.
    // <param name="index">The specified index of the attribute.</param>
    // <param name="nameLength">Outputs the string length of the name of the specified
    // attribute.</param>
    // <param name="inherited">Outputs whether the attribute is set to the 'inherit'
    // value.</param>
    function GetSpecifiedAttributeNameLength(index: UINT32;
                                             out nameLength: UINT32;
                                             {out} _inherited: PBOOL = Nil): HResult; stdcall;

    // Removes the attribute from this element. Also removes this attribute from within
    // an inline style if present. Returns an error if the attribute name is not valid
    // on this element.
    function RemoveAttribute(name: LPWSTR): HResult; stdcall;

    // Sets the value of a text content element.
    function SetTextValue(name: PWideChar;
                          nameCount: UINT32): HResult; stdcall;

    // Gets the value of a text content element.
    function GetTextValue(out name: PWideChar;
                          nameCount: UINT32): HResult; stdcall;

    // Gets the length of the text content value. The returned string length does not
    // include room for the null terminator.
    function GetTextValueLength(): UINT32; stdcall;

    // Sets an attribute of this element using an interface. Returns an error if the
    // attribute name is not valid on this element. Returns an error if the attribute
    // cannot be expressed as the specified interface type. Returns an error if the
    // attribute object is already set on an element. A given attribute object may only
    // be set on one element in one attribute location at a time.
    function SetAttributeValue(name: LPWSTR;
                               value: ID2D1SvgAttribute): HResult; overload; stdcall;

    // Sets an attribute of this element using a POD type. Returns an error if the
    // attribute name is not valid on this element. Returns an error if the attribute
    // cannot be expressed as the specified type.
    function SetAttributeValue(name: LPWSTR;
                               _type: D2D1_SVG_ATTRIBUTE_POD_TYPE;
                               value: Pointer;
                               valueSizeInBytes: UINT32): HResult; overload; stdcall;

    // Sets an attribute of this element using a string. Returns an error if the
    // attribute name is not valid on this element. Returns an error if the attribute
    // cannot be expressed as the specified type.
    function SetAttributeValue(name: LPWSTR;
                               _type: D2D1_SVG_ATTRIBUTE_STRING_TYPE;
                               value: LPWSTR): HResult; overload; stdcall;

    // Gets an attribute of this element as an interface type. Returns an error if the
    // attribute is not specified. Returns an error if the attribute name is not valid
    // on this element. Returns an error if the attribute cannot be expressed as the
    // specified interface type.
    // <param name="riid">The interface ID of the attribute value.</param>
    function GetAttributeValue(name: LPWSTR;
                               const riid: TGUID;
                               var value: Pointer): HResult; overload; stdcall;

    // Gets an attribute of this element as a POD type. Returns an error if the
    // attribute is not specified. Returns an error if the attribute name is not valid
    // on this element. Returns an error if the attribute cannot be expressed as the
    // specified POD type.
    function GetAttributeValue(name: LPWSTR;
                               _type: D2D1_SVG_ATTRIBUTE_POD_TYPE;
                               value: Pointer;
                               valueSizeInBytes: UINT32): HResult; overload; stdcall;

    // Gets an attribute of this element as a string. Returns an error if the attribute
    // is not specified. Returns an error if the attribute name is not valid on this
    // element. Returns an error if the attribute cannot be expressed as the specified
    // string type.
    function GetAttributeValue(name: LPWSTR;
                               _type: D2D1_SVG_ATTRIBUTE_STRING_TYPE;
                               value: PWideChar;
                               valueCount: UINT32): HResult; overload; stdcall;

    // Gets the string length of an attribute of this element. The returned string
    // length does not include room for the null terminator. Returns an error if the
    // attribute is not specified. Returns an error if the attribute name is not valid
    // on this element. Returns an error if the attribute cannot be expressed as the
    // specified string type.
    function GetAttributeValueLength(name: LPWSTR;
                                     _type: D2D1_SVG_ATTRIBUTE_STRING_TYPE;
                                     out valueLength: UINT32): HResult; stdcall;


  end;
  IID_ID2D1SvgElement = ID2D1SvgElement;
  {$EXTERNALSYM IID_ID2D1SvgElement}


  // Interface ID2D1SvgDocument
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1SvgDocument);'}
  {$EXTERNALSYM ID2D1SvgDocument}
  ID2D1SvgDocument = interface(ID2D1Resource)
   ['{86b88e4d-afa4-4d7b-88e4-68a51c4a0aec}']

    // Sets the size of the initial viewport.
    function SetViewportSize(viewportSize: D2D1_SIZE_F): HResult; stdcall;

    // Returns the size of the initial viewport.
    procedure GetViewportSize(out size: D2D1_SIZE_F); stdcall;

    // Sets the root element of the document. The root element must be an 'svg'
    // element. If the element already exists within an svg tree, it is first removed.
    function SetRoot({In_opt} root: ID2D1SvgElement): HResult; stdcall;

    // Gets the root element of the document.
    procedure GetRoot(out root: ID2D1SvgElement); stdcall;

    // Gets the SVG element with the specified ID. If the element cannot be found, the
    // returned element will be Nil.
    function FindElementById(id: LPWSTR;
                             out svgElement: ID2D1SvgElement): HResult; stdcall;

    // Serializes an element and its subtree to XML. The output XML is encoded as
    // UTF-8.
    // <param name="outputXmlStream">An output stream to contain the SVG XML subtree.
    // </param>
    // <param name="subtree">The root of the subtree. If null, the entire document is
    // serialized.</param>
    function Serialize(outputXmlStream: IStream;
                       {in_opt} subtree: PID2D1SvgElement = Nil): HResult; stdcall;


    // Deserializes a subtree from the stream. The stream must have only one root
    // element, but that root element need not be an 'svg' element. The output element
    // is not inserted into this document tree.
    // <param name="inputXmlStream">An input stream containing the SVG XML subtree.
    // </param>
    // <param name="subtree">The root of the subtree.</param>
    function Deserialize(inputXmlStream: IStream;
                         out subtree: ID2D1SvgElement): HResult; stdcall;

    // Creates a paint object which can be used to set the 'fill' or 'stroke'
    // properties.
    // <param name="color">The color used if the paintType is
    // D2D1_SVG_PAINT_TYPE_COLOR.</param>
    // <param name="id">The element id which acts as the paint server. This id is used
    // if the paint type is D2D1_SVG_PAINT_TYPE_URI.</param>
    function CreatePaint(paintType: D2D1_SVG_PAINT_TYPE;
                         {in_opt} color: PD2D1COLORF;
                         {in_opt} id: LPWSTR;
                         out paint: ID2D1SvgPaint): HResult; stdcall;

    // Creates a dash array object which can be used to set the 'stroke-dasharray'
    // property.
    function CreateStrokeDashArray(dashes: PD2D1_SVG_LENGTH;
                                   dashesCount: UINT32;
                                   out strokeDashArray: ID2D1SvgStrokeDashArray): HResult; stdcall;

    // Creates a points object which can be used to set a 'points' attribute on a
    // 'polygon' or 'polyline' element.
    function CreatePointCollection(points: PD2D1POINT2F;
                                   pointsCount: UINT32;
                                   out pointCollection: ID2D1SvgPointCollection): HResult; stdcall;

    // Creates a path data object which can be used to set a 'd' attribute on a 'path'
    // element.
    function CreatePathData(segmentData: PSingle;
                            segmentDataCount: UINT32;
                            commands: PD2D1_SVG_PATH_COMMAND;
                            commandsCount: UINT32;
                            out pathData: ID2D1SvgPathData): HResult; stdcall;

  end;
  IID_ID2D1SvgDocument = ID2D1SvgDocument;
  {$EXTERNALSYM IID_ID2D1SvgDocument}



{$EndRegion ID2D1SvgDocument}



   // Interface ID2D1DeviceContext5
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1DeviceContext5);'}
  {$EXTERNALSYM ID2D1DeviceContext5}
  ID2D1DeviceContext5 = interface(ID2D1DeviceContext4)
  ['{7836d248-68cc-4df6-b9e8-de991bf62eb7}']

    // Creates an SVG document from a stream.

    // <param name="inputXmlStream">An input stream containing the SVG XML document. If
    // nil, an empty document is created.</param>
    // <param name="viewportSize">Size of the initial viewport of the document.</param>
    // <param name="svgDocument">When this method returns, contains a pointer to the
    // SVG document.</param>
    function CreateSvgDocument({in_opt} inputXmlStream: IStream;
                               viewportSize: D2D1_SIZE_F;
                               out svgDocument: ID2D1SvgDocument): HResult; stdcall;

    // Draw an SVG document.
    procedure DrawSvgDocument(svgDocument: ID2D1SvgDocument); stdcall;

    // Creates a color context from a DXGI color space type. It is only valid to use
    // this with the Color Management Effect in 'Best' mode.
    function CreateColorContextFromDxgiColorSpace(colorSpace: DXGI_COLOR_SPACE_TYPE;
                                                  out colorContext: ID2D1ColorContext1): HResult; stdcall;

    // Creates a color context from a simple color profile. It is only valid to use
    // this with the Color Management Effect in 'Best' mode.
    function CreateColorContextFromSimpleColorProfile(simpleProfile: D2D1_SIMPLE_COLOR_PROFILE;
                                                      out colorContext: ID2D1ColorContext1): HResult; stdcall;
  end;
  IID_ID2D1DeviceContext5 = ID2D1DeviceContext5;
  {$EXTERNALSYM IID_ID2D1DeviceContext5}

  IWICImageEncoder = interface(IUnknown)
    ['{04C75BF8-3CE1-473B-ACC5-3CC4F5E94999}']
    function WriteFrame(pImage: ID2D1Image; pFrameEncode: IWICBitmapFrameEncode; const pImageParameters: TWICImageParameters): HResult; stdcall;
    function WriteFrameThumbnail(pImage: ID2D1Image; pFrameEncode: IWICBitmapFrameEncode;
      const pImageParameters: TWICImageParameters): HResult; stdcall;
    function WriteThumbnail(pImage: ID2D1Image; pEncoder: IWICBitmapEncoder; const pImageParameters: TWICImageParameters): HResult; stdcall;
  end;
  {$EXTERNALSYM IWICImageEncoder}

    IWICImagingFactory = interface(IUnknown)
        ['{ec5ec8a9-c395-4314-9c77-54d7a935ff70}']
        function CreateDecoderFromFilename(wzFilename: PWideChar; pguidVendor: PGUID; // const ?!?
            dwDesiredAccess: DWORD; metadataOptions: WICDecodeOptions; out ppIDecoder: IWICBitmapDecoder): HResult; stdcall;
        function CreateDecoderFromStream(pIStream: IStream; pguidVendor: PGUID; metadataOptions: WICDecodeOptions;
            out ppIDecoder: IWICBitmapDecoder): HResult; stdcall;
        function CreateDecoderFromFileHandle(hFile: Pointer; pguidVendor: PGUID; metadataOptions: WICDecodeOptions;
            out ppIDecoder: IWICBitmapDecoder): HResult; stdcall;
        function CreateComponentInfo(clsidComponent: TGUID; out ppIInfo: IWICComponentInfo): HResult; stdcall;
        function CreateDecoder(const guidContainerFormat: TGUID; pguidVendor: PGUID; out ppIDecoder: IWICBitmapDecoder): HResult; stdcall;
        function CreateEncoder(const guidContainerFormat: TGUID; pguidVendor: PGUID; out ppIEncoder: IWICBitmapEncoder): HResult; stdcall;
        function CreatePalette(out ppIPalette: IWICPalette): HResult; stdcall;
        function CreateFormatConverter(out ppIFormatConverter: IWICFormatConverter): HResult; stdcall;
        function CreateBitmapScaler(out ppIBitmapScaler: IWICBitmapScaler): HResult; stdcall;
        function CreateBitmapClipper(out ppIBitmapClipper: IWICBitmapClipper): HResult; stdcall;
        function CreateBitmapFlipRotator(out ppIBitmapFlipRotator: IWICBitmapFlipRotator): HResult; stdcall;
        function CreateStream(out ppIWICStream: IWICStream): HResult; stdcall;
        function CreateColorContext(out ppIWICColorContext: IWICColorContext): HResult; stdcall;
        function CreateColorTransformer(out ppIWICColorTransform: IWICColorTransform): HResult; stdcall;
        function CreateBitmap(uiWidth: UINT; uiHeight: UINT; const pixelFormat: REFWICPixelFormatGUID;
            option: WICBitmapCreateCacheOption; out ppIBitmap: IWICBitmap): HResult; stdcall;
        function CreateBitmapFromSource(pIBitmapSource: IWICBitmapSource; option: WICBitmapCreateCacheOption;
            out ppIBitmap: IWICBitmap): HResult; stdcall;
        function CreateBitmapFromSourceRect(pIBitmapSource: IWICBitmapSource; x: UINT; y: UINT; Width: UINT;
            Height: UINT; out ppIBitmap: IWICBitmap): HResult; stdcall;
        function CreateBitmapFromMemory(uiWidth: UINT; uiHeight: UINT; const pixelFormat: REFWICPixelFormatGUID;
            cbStride: UINT; cbBufferSize: UINT; pbBuffer: PBYTE; out ppIBitmap: IWICBitmap): HResult; stdcall;
        function CreateBitmapFromHBITMAP(hBitmap: HBITMAP; hPalette: HPALETTE; options: WICBitmapAlphaChannelOption;
            out ppIBitmap: IWICBitmap): HResult; stdcall;
        function CreateBitmapFromHICON(hIcon: HICON; out ppIBitmap: IWICBitmap): HResult; stdcall;
        function CreateComponentEnumerator(componentTypes: DWORD; options: DWORD; out ppIEnumUnknown: IEnumUnknown): HResult; stdcall;
        function CreateFastMetadataEncoderFromDecoder(pIDecoder: IWICBitmapDecoder;
            out ppIFastEncoder: IWICFastMetadataEncoder): HResult; stdcall;
        function CreateFastMetadataEncoderFromFrameDecode(pIFrameDecoder: IWICBitmapFrameDecode;
            out ppIFastEncoder: IWICFastMetadataEncoder): HResult; stdcall;
        function CreateQueryWriter(guidMetadataFormat: TGUID; pguidVendor: PGUID;
            out ppIQueryWriter: IWICMetadataQueryWriter): HResult; stdcall;
        function CreateQueryWriterFromReader(pIQueryReader: IWICMetadataQueryReader; pguidVendor: PGUID;
            out ppIQueryWriter: IWICMetadataQueryWriter): HResult; stdcall;
    end;

  { part of WinCodec.h, but cross reference problem }
  IWICImagingFactory2 = interface(IWICImagingFactory)
    ['{7B816B45-1996-4476-B132-DE9E247C8AF0}']
    function CreateImageEncoder(pD2DDevice: ID2D1Device; out ppWICImageEncoder: IWICImageEncoder): HResult; stdcall;
  end;
  {$EXTERNALSYM IWICImagingFactory2}

{$WARN BOUNDS_ERROR OFF}

const
    CLSID_WICImagingFactory2: TGUID = '{317d06e8-5f24-433d-bdf7-79ce68d8abc2}';

    { D2D1Effects.h }
    // Built in effect CLSIDs
    CLSID_D2D12DAffineTransform: TGUID = '{6AA97485-6354-4cfc-908C-E4A74F62C96C}';
    CLSID_D2D13DPerspectiveTransform: TGUID = '{C2844D0B-3D86-46e7-85BA-526C9240F3FB}';
    CLSID_D2D13DTransform: TGUID = '{e8467b04-ec61-4b8a-b5de-d4d73debea5a}';
    CLSID_D2D1ArithmeticComposite: TGUID = '{fc151437-049a-4784-a24a-f1c4daf20987}';
    CLSID_D2D1Atlas: TGUID = '{913e2be4-fdcf-4fe2-a5f0-2454f14ff408}';
    CLSID_D2D1BitmapSource: TGUID = '{5fb6c24d-c6dd-4231-9404-50f4d5c3252d}';
    CLSID_D2D1Blend: TGUID = '{81c5b77b-13f8-4cdd-ad20-c890547ac65d}';
    CLSID_D2D1Border: TGUID = '{2A2D49C0-4ACF-43c7-8C6A-7C4A27874D27}';
    CLSID_D2D1Brightness: TGUID = '{8cea8d1e-77b0-4986-b3b9-2f0c0eae7887}';
    CLSID_D2D1ColorManagement: TGUID = '{1A28524C-FDD6-4AA4-AE8F-837EB8267B37}';
    CLSID_D2D1ColorMatrix: TGUID = '{921F03D6-641C-47DF-852D-B4BB6153AE11}';
    CLSID_D2D1Composite: TGUID = '{48fc9f51-f6ac-48f1-8b58-3b28ac46f76d}';
    CLSID_D2D1ConvolveMatrix: TGUID = '{407f8c08-5533-4331-a341-23cc3877843e}';
    CLSID_D2D1Crop: TGUID = '{E23F7110-0E9A-4324-AF47-6A2C0C46F35B}';
    CLSID_D2D1DirectionalBlur: TGUID = '{174319a6-58e9-49b2-bb63-caf2c811a3db}';
    CLSID_D2D1DiscreteTransfer: TGUID = '{90866fcd-488e-454b-af06-e5041b66c36c}';
    CLSID_D2D1DisplacementMap: TGUID = '{edc48364-0417-4111-9450-43845fa9f890}';
    CLSID_D2D1DistantDiffuse: TGUID = '{3e7efd62-a32d-46d4-a83c-5278889ac954}';
    CLSID_D2D1DistantSpecular: TGUID = '{428c1ee5-77b8-4450-8ab5-72219c21abda}';
    CLSID_D2D1DpiCompensation: TGUID = '{6c26c5c7-34e0-46fc-9cfd-e5823706e228}';
    CLSID_D2D1Flood: TGUID = '{61c23c20-ae69-4d8e-94cf-50078df638f2}';
    CLSID_D2D1GammaTransfer: TGUID = '{409444c4-c419-41a0-b0c1-8cd0c0a18e42}';
    CLSID_D2D1GaussianBlur: TGUID = '{1feb6d69-2fe6-4ac9-8c58-1d7f93e7a6a5}';
    CLSID_D2D1Scale: TGUID = '{9daf9369-3846-4d0e-a44e-0c607934a5d7}';
    CLSID_D2D1Histogram: TGUID = '{881db7d0-f7ee-4d4d-a6d2-4697acc66ee8}';
    CLSID_D2D1HueRotation: TGUID = '{0f4458ec-4b32-491b-9e85-bd73f44d3eb6}';
    CLSID_D2D1LinearTransfer: TGUID = '{ad47c8fd-63ef-4acc-9b51-67979c036c06}';
    CLSID_D2D1LuminanceToAlpha: TGUID = '{41251ab7-0beb-46f8-9da7-59e93fcce5de}';
    CLSID_D2D1Morphology: TGUID = '{eae6c40d-626a-4c2d-bfcb-391001abe202}';
    CLSID_D2D1OpacityMetadata: TGUID = '{6c53006a-4450-4199-aa5b-ad1656fece5e}';
    CLSID_D2D1PointDiffuse: TGUID = '{b9e303c3-c08c-4f91-8b7b-38656bc48c20}';
    CLSID_D2D1PointSpecular: TGUID = '{09c3ca26-3ae2-4f09-9ebc-ed3865d53f22}';
    CLSID_D2D1Premultiply: TGUID = '{06eab419-deed-4018-80d2-3e1d471adeb2}';
    CLSID_D2D1Saturation: TGUID = '{5cb2d9cf-327d-459f-a0ce-40c0b2086bf7}';
    CLSID_D2D1Shadow: TGUID = '{C67EA361-1863-4e69-89DB-695D3E9A5B6B}';
    CLSID_D2D1SpotDiffuse: TGUID = '{818a1105-7932-44f4-aa86-08ae7b2f2c93}';
    CLSID_D2D1SpotSpecular: TGUID = '{edae421e-7654-4a37-9db8-71acc1beb3c1}';
    CLSID_D2D1TableTransfer: TGUID = '{5bf818c3-5e43-48cb-b631-868396d6a1d4}';
    CLSID_D2D1Tile: TGUID = '{B0784138-3B76-4bc5-B13B-0FA2AD02659F}';
    CLSID_D2D1Turbulence: TGUID = '{CF2BB6AE-889A-4ad7-BA29-A2FD732C9FC9}';
    CLSID_D2D1UnPremultiply: TGUID = '{fb9ac489-ad8d-41ed-9999-bb6347d110f7}';

    // Built in effect CLSIDs
    CLSID_D2D1Contrast: TGUID = '{b648a78a-0ed5-4f80-a94a-8e825aca6b77}';
    CLSID_D2D1RgbToHue: TGUID = '{23f3e5ec-91e8-4d3d-ad0a-afadc1004aa1}';
    CLSID_D2D1HueToRgb: TGUID = '{7b78a6bd-0141-4def-8a52-6356ee0cbdd5}';
    CLSID_D2D1ChromaKey: TGUID = '{74C01F5B-2A0D-408C-88E2-C7A3C7197742}';
    CLSID_D2D1Emboss: TGUID = '{b1c5eb2b-0348-43f0-8107-4957cacba2ae}';
    CLSID_D2D1Exposure: TGUID = '{b56c8cfa-f634-41ee-bee0-ffa617106004}';
    CLSID_D2D1Grayscale: TGUID = '{36DDE0EB-3725-42E0-836D-52FB20AEE644}';
    CLSID_D2D1Invert: TGUID = '{e0c3784d-cb39-4e84-b6fd-6b72f0810263}';
    CLSID_D2D1Posterize: TGUID = '{2188945e-33a3-4366-b7bc-086bd02d0884}';
    CLSID_D2D1Sepia: TGUID = '{3a1af410-5f1d-4dbe-84df-915da79b7153}';
    CLSID_D2D1Sharpen: TGUID = '{C9B887CB-C5FF-4DC5-9779-273DCF417C7D}';
    CLSID_D2D1Straighten: TGUID = '{4da47b12-79a3-4fb0-8237-bbc3b2a4de08}';
    CLSID_D2D1TemperatureTint: TGUID = '{89176087-8AF9-4A08-AEB1-895F38DB1766}';
    CLSID_D2D1Vignette: TGUID = '{c00c40be-5e67-4ca3-95b4-f4b02c115135}';
    CLSID_D2D1EdgeDetection: TGUID = '{EFF583CA-CB07-4AA9-AC5D-2CC44C76460F}';
    CLSID_D2D1HighlightsShadows: TGUID = '{CADC8384-323F-4C7E-A361-2E2B24DF6EE4}';
    CLSID_D2D1LookupTable3D: TGUID = '{349E0EDA-0088-4A79-9CA3-C7E300202020}';
    CLSID_D2D1Opacity: TGUID = '{811d79a4-de28-4454-8094-c64685f8bd4c}';
    CLSID_D2D1AlphaMask: TGUID = '{c80ecff0-3fd5-4f05-8328-c5d1724b4f0a}';
    CLSID_D2D1CrossFade: TGUID = '{12f575e8-4db1-485f-9a84-03a07dd3829f}';
    CLSID_D2D1Tint: TGUID = '{36312b17-f7dd-4014-915d-ffca768cf211}';

type
  TD2D_VECTOR_2F = record
    x: single;
    y: single;
  end;
  TD2D1_VECTOR_2F = TD2D_VECTOR_2F;

  TD2D_VECTOR_3F = record
    x: single;
    y: single;
    z: single;
  end;
  TD2D1_VECTOR_3F = TD2D_VECTOR_3F;

  TD2D_VECTOR_4F = record
    x: single;
    y: single;
    z: single;
    w: single;
  end;
  TD2D1_VECTOR_4F = TD2D_VECTOR_4F;

  { TD2D_MATRIX_5X4_F }

  TD2D_MATRIX_5X4_F = record
      procedure Init; overload;
      procedure Init(
        m11, m12, m13, m14,
        m21, m22, m23, m24,
        m31, m32, m33, m34,
        m41, m42, m43, m44,
        m51, m52, m53, m54: single);
            overload;
      case integer of
            0: (_11, _12, _13, _14: single;
                _21, _22, _23, _24: single;
                _31, _32, _33, _34: single;
                _41, _42, _43, _44: single;
                _51, _52, _53, _54: single;);
            1: (m: array [0 .. 4, 0 .. 3] of single;);
  end;

  TD2D1_GAUSSIANBLUR_PROP = (
    // Property Name: "StandardDeviation"
    // Property Type: FLOAT
    D2D1_GAUSSIANBLUR_PROP_STANDARD_DEVIATION = 0,
    // Property Name: "Optimization"
    // Property Type: D2D1_GAUSSIANBLUR_OPTIMIZATION
    D2D1_GAUSSIANBLUR_PROP_OPTIMIZATION = 1,
    // Property Name: "BorderMode"
    // Property Type: D2D1_BORDER_MODE
    D2D1_GAUSSIANBLUR_PROP_BORDER_MODE = 2,
    D2D1_GAUSSIANBLUR_PROP_FORCE_DWORD = $ffffffff
  );

  TD2D1_SHADOW_PROP = (
    // Property Name: "BlurStandardDeviation"
    // Property Type: FLOAT
    D2D1_SHADOW_PROP_BLUR_STANDARD_DEVIATION = 0,
    // Property Name: "Color"
    // Property Type: D2D1_VECTOR_4F
    D2D1_SHADOW_PROP_COLOR = 1,
    // Property Name: "Optimization"
    // Property Type: D2D1_SHADOW_OPTIMIZATION
    D2D1_SHADOW_PROP_OPTIMIZATION = 2,
    D2D1_SHADOW_PROP_FORCE_DWORD = $ffffffff
  );

  TD2D1_SHADOW_OPTIMIZATION = (
    D2D1_SHADOW_OPTIMIZATION_SPEED = 0,
    D2D1_SHADOW_OPTIMIZATION_BALANCED = 1,
    D2D1_SHADOW_OPTIMIZATION_QUALITY = 2,
    D2D1_SHADOW_OPTIMIZATION_FORCE_DWORD = $ffffffff
  );

  TD2D1_2DAFFINETRANSFORM_PROP = (
    D2D1_2DAFFINETRANSFORM_PROP_INTERPOLATION_MODE = 0,
    D2D1_2DAFFINETRANSFORM_PROP_BORDER_MODE = 1,
    D2D1_2DAFFINETRANSFORM_PROP_TRANSFORM_MATRIX = 2,
    D2D1_2DAFFINETRANSFORM_PROP_SHARPNESS = 3,
    D2D1_2DAFFINETRANSFORM_PROP_FORCE_DWORD = $ffffffff
  );

  TD2D1_3DPERSPECTIVETRANSFORM_PROP = (
    D2D1_3DPERSPECTIVETRANSFORM_PROP_INTERPOLATION_MODE = 0,
    D2D1_3DPERSPECTIVETRANSFORM_PROP_BORDER_MODE = 1,
    D2D1_3DPERSPECTIVETRANSFORM_PROP_DEPTH = 2,
    D2D1_3DPERSPECTIVETRANSFORM_PROP_PERSPECTIVE_ORIGIN = 3,
    D2D1_3DPERSPECTIVETRANSFORM_PROP_LOCAL_OFFSET = 4,
    D2D1_3DPERSPECTIVETRANSFORM_PROP_GLOBAL_OFFSET = 5,
    D2D1_3DPERSPECTIVETRANSFORM_PROP_ROTATION_ORIGIN = 6,
    D2D1_3DPERSPECTIVETRANSFORM_PROP_ROTATION = 7,
    D2D1_3DPERSPECTIVETRANSFORM_PROP_FORCE_DWORD = $ffffffff
  );

  TD2D1_3DPERSPECTIVETRANSFORM_INTERPOLATION_MODE = (
    D2D1_3DPERSPECTIVETRANSFORM_INTERPOLATION_MODE_NEAREST_NEIGHBOR = 0,
    D2D1_3DPERSPECTIVETRANSFORM_INTERPOLATION_MODE_LINEAR = 1,
    D2D1_3DPERSPECTIVETRANSFORM_INTERPOLATION_MODE_CUBIC = 2,
    D2D1_3DPERSPECTIVETRANSFORM_INTERPOLATION_MODE_MULTI_SAMPLE_LINEAR = 3,
    D2D1_3DPERSPECTIVETRANSFORM_INTERPOLATION_MODE_ANISOTROPIC = 4,
    D2D1_3DPERSPECTIVETRANSFORM_INTERPOLATION_MODE_FORCE_DWORD = $ffffffff
  );

  TD2D1_BORDER_MODE = (
    D2D1_BORDER_MODE_SOFT = 0,
    D2D1_BORDER_MODE_HARD = 1,
    D2D1_BORDER_MODE_FORCE_DWORD = $ffffffff);

  TD2D1_BRIGHTNESS_PROP = (
    D2D1_BRIGHTNESS_PROP_WHITE_POINT = 0,
    D2D1_BRIGHTNESS_PROP_BLACK_POINT = 1,
    D2D1_BRIGHTNESS_PROP_FORCE_DWORD = $ffffffff
    );

  // The enumeration of the Saturation effect's top level properties.
  // Effect description: Alters the saturation of the bitmap based on the user
  // specified saturation value.
  TD2D1_SATURATION_PROP = (
    // Property Name: "Saturation"
    // Property Type: FLOAT
    D2D1_SATURATION_PROP_SATURATION = 0,
    D2D1_SATURATION_PROP_FORCE_DWORD = $ffffffff
    );

  // The enumeration of the Hue Rotation effect's top level properties.
  // Effect description: Changes the Hue of a bitmap based on a user specified Hue
  // Rotation angle.

  TD2D1_HUEROTATION_PROP = (
    // Property Name: "Angle"
    // Property Type: FLOAT
    D2D1_HUEROTATION_PROP_ANGLE = 0,
    D2D1_HUEROTATION_PROP_FORCE_DWORD = $ffffffff
    );


  TD2D1_COLORMATRIX_PROP = (
    D2D1_COLORMATRIX_PROP_COLOR_MATRIX = 0,
    D2D1_COLORMATRIX_PROP_ALPHA_MODE = 1,
    D2D1_COLORMATRIX_PROP_CLAMP_OUTPUT = 2,
    D2D1_COLORMATRIX_PROP_FORCE_DWORD = $ffffffff
    );

  TD2D1_COLORMATRIX_ALPHA_MODE = (
    D2D1_COLORMATRIX_ALPHA_MODE_PREMULTIPLIED = 1,
    D2D1_COLORMATRIX_ALPHA_MODE_STRAIGHT = 2,
    D2D1_COLORMATRIX_ALPHA_MODE_FORCE_DWORD = $ffffffff
    );

	// The enumeration of the Contrast effect's top level properties.
	// Effect description: Adjusts the contrast of an image.
  TD2D1_CONTRAST_PROP = (
    // Property Name: "Contrast"
    // Property Type: FLOAT
    D2D1_CONTRAST_PROP_CONTRAST = 0,
    // Property Name: "ClampInput"
    // Property Type: BOOL
    D2D1_CONTRAST_PROP_CLAMP_INPUT = 1,
    D2D1_CONTRAST_PROP_FORCE_DWORD = $ffffffff
    );

	// The enumeration of the Sharpen effect's top level properties.
	// Effect description: Performs sharpening adjustment
  TD2D1_SHARPEN_PROP = (
    // Property Name: "Sharpness"
    // Property Type: FLOAT
    D2D1_SHARPEN_PROP_SHARPNESS = 0,
    // Property Name: "Threshold"
    // Property Type: FLOAT
    D2D1_SHARPEN_PROP_THRESHOLD = 1,
    D2D1_SHARPEN_PROP_FORCE_DWORD = $ffffffff
    );

	// The enumeration of the Exposure effect's top level properties.
	// Effect description: Simulates camera exposure adjustment.
  TD2D1_EXPOSURE_PROP = (
    // Property Name: "ExposureValue"
    // Property Type: FLOAT
    D2D1_EXPOSURE_PROP_EXPOSURE_VALUE = 0,
    D2D1_EXPOSURE_PROP_FORCE_DWORD = $ffffffff
  );

	// The enumeration of the Highlights and Shadows effect's top level properties.
	// Effect description: Adjusts the highlight and shadow strength of an image.

  TD2D1_HIGHLIGHTSANDSHADOWS_PROP = (
    // Property Name: "Highlights"
    // Property Type: FLOAT
    D2D1_HIGHLIGHTSANDSHADOWS_PROP_HIGHLIGHTS = 0,
    // Property Name: "Shadows"
    // Property Type: FLOAT
    D2D1_HIGHLIGHTSANDSHADOWS_PROP_SHADOWS = 1,
    // Property Name: "Clarity"
    // Property Type: FLOAT
    D2D1_HIGHLIGHTSANDSHADOWS_PROP_CLARITY = 2,
    // Property Name: "InputGamma"
    // Property Type: D2D1_HIGHLIGHTSANDSHADOWS_INPUT_GAMMA
    D2D1_HIGHLIGHTSANDSHADOWS_PROP_INPUT_GAMMA = 3,
    // Property Name: "MaskBlurRadius"
    // Property Type: FLOAT
    D2D1_HIGHLIGHTSANDSHADOWS_PROP_MASK_BLUR_RADIUS = 4,
    D2D1_HIGHLIGHTSANDSHADOWS_PROP_FORCE_DWORD = $ffffffff
    );

  TD2D1_HIGHLIGHTSANDSHADOWS_INPUT_GAMMA = (
    D2D1_HIGHLIGHTSANDSHADOWS_INPUT_GAMMA_LINEAR = 0,
    D2D1_HIGHLIGHTSANDSHADOWS_INPUT_GAMMA_SRGB = 1,
    D2D1_HIGHLIGHTSANDSHADOWS_INPUT_GAMMA_FORCE_DWORD = $ffffffff
    );

	// The enumeration of the Straighten effect's top level properties.
	// Effect description: Straightens an image.
  TD2D1_STRAIGHTEN_PROP = (
    // Property Name: "Angle"
    // Property Type: FLOAT
    D2D1_STRAIGHTEN_PROP_ANGLE = 0,
    // Property Name: "MaintainSize"
    // Property Type: BOOL
    D2D1_STRAIGHTEN_PROP_MAINTAIN_SIZE = 1,
    // Property Name: "ScaleMode"
    // Property Type: D2D1_STRAIGHTEN_SCALE_MODE
    D2D1_STRAIGHTEN_PROP_SCALE_MODE = 2,
    D2D1_STRAIGHTEN_PROP_FORCE_DWORD = $ffffffff
    );

  TD2D1_STRAIGHTEN_SCALE_MODE = (
    D2D1_STRAIGHTEN_SCALE_MODE_NEAREST_NEIGHBOR = 0,
    D2D1_STRAIGHTEN_SCALE_MODE_LINEAR = 1,
    D2D1_STRAIGHTEN_SCALE_MODE_CUBIC = 2,
    D2D1_STRAIGHTEN_SCALE_MODE_MULTI_SAMPLE_LINEAR = 3,
    D2D1_STRAIGHTEN_SCALE_MODE_ANISOTROPIC = 4,
    D2D1_STRAIGHTEN_SCALE_MODE_FORCE_DWORD = $ffffffff
    );

	// The enumeration of the Temperature And Tint effect's top level properties.
	// Effect description: Adjusts the temperature and tint of an image.
  TD2D1_TEMPERATUREANDTINT_PROP = (
    // Property Name: "Temperature"
    // Property Type: FLOAT
    D2D1_TEMPERATUREANDTINT_PROP_TEMPERATURE = 0,
    // Property Name: "Tint"
    // Property Type: FLOAT
    D2D1_TEMPERATUREANDTINT_PROP_TINT = 1,
    D2D1_TEMPERATUREANDTINT_PROP_FORCE_DWORD = $ffffffff
    );

	// The enumeration of the Sepia effect's top level properties.
	// Effect description: Applies a Sepia tone to an image.
  TD2D1_SEPIA_PROP = (
     // Property Name: "Intensity"
     // Property Type: FLOAT
     D2D1_SEPIA_PROP_INTENSITY = 0,
     // Property Name: "AlphaMode"
     // Property Type: D2D1_ALPHA_MODE
     D2D1_SEPIA_PROP_ALPHA_MODE = 1,
     D2D1_SEPIA_PROP_FORCE_DWORD = $ffffffff
     );

  {TD2D1_ALPHA_MODE = (
    D2D1_ALPHA_MODE_UNKNOWN = 0,
    D2D1_ALPHA_MODE_PREMULTIPLIED = 1,
    D2D1_ALPHA_MODE_STRAIGHT = 2,
    D2D1_ALPHA_MODE_IGNORE = 3,
    D2D1_ALPHA_MODE_FORCE_DWORD = $FFFFFFFF
    ); }

	// The enumeration of the Vignette effect's top level properties.
	// Effect description: Fades the edges of an image to the specified color.
  TD2D1_VIGNETTE_PROP = (
    // Property Name: "Color"
    // Property Type: D2D1_VECTOR_4F
    D2D1_VIGNETTE_PROP_COLOR = 0,
    // Property Name: "TransitionSize"
    // Property Type: FLOAT
    D2D1_VIGNETTE_PROP_TRANSITION_SIZE = 1,
    // Property Name: "Strength"
    // Property Type: FLOAT
    D2D1_VIGNETTE_PROP_STRENGTH = 2,
    D2D1_VIGNETTE_PROP_FORCE_DWORD = $ffffffff
    );

  TD2D1_FLOOD_PROP = (
    D2D1_FLOOD_PROP_COLOR = 0,
    D2D1_FLOOD_PROP_FORCE_DWORD = $ffffffff
    );

function Vector2F(x: single = 0.0; y: single= 0.0): TD2D1_VECTOR_2F;
function Vector3F(x: single = 0.0; y: single = 0.0; z: single = 0.0): TD2D1_VECTOR_3F;
function Vector4F(x: single = 0.0; y: single = 0.0; z: single = 0.0; w: single = 0.0): TD2D1_VECTOR_4F;

function DWTextRange(start, length: integer) : TDWriteTextRange;

procedure SetInputEffect(effect: ID2D1Effect; index: UINT32;
  inputEffect: ID2D1Effect; invalidate: longbool=TRUE);

function D2D1BitmapProperties1(const ABitmapOptions: D2D1_BITMAP_OPTIONS; const APixelFormat: TD2D1PixelFormat;
  const ADpiX: Single = 0; const ADpiY: Single = 0; const AColorContext: ID2D1ColorContext = nil): D2D1_BITMAP_PROPERTIES1;

implementation

function Vector2F(x: single; y: single): TD2D1_VECTOR_2F;
begin
  Result.x := x;
  Result.y := y;
end;

function Vector3F(x: single; y: single; z: single): TD2D1_VECTOR_3F;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;

function Vector4F(x: single; y: single; z: single; w: single): TD2D1_VECTOR_4F;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
  Result.w := w;
end;

{ TD2D_MATRIX_5X4_F }

procedure TD2D_MATRIX_5X4_F.Init;
begin
    _11 := 1;
    _12 := 0;
    _13 := 0;
    _14 := 0;

    _21 := 0;
    _22 := 1;
    _23 := 0;
    _24 := 0;

    _31 := 0;
    _32 := 0;
    _33 := 1;
    _34 := 0;

    _41 := 0;
    _42 := 0;
    _43 := 0;
    _44 := 1;

    _51 := 0;
    _52 := 0;
    _53 := 0;
    _54 := 0;
end;



procedure TD2D_MATRIX_5X4_F.Init(m11, m12, m13, m14, m21, m22, m23, m24, m31, m32, m33, m34, m41, m42, m43, m44, m51, m52, m53, m54: single);
begin
    _11 := m11;
    _12 := m12;
    _13 := m13;
    _14 := m14;

    _21 := m21;
    _22 := m22;
    _23 := m23;
    _24 := m24;

    _31 := m31;
    _32 := m32;
    _33 := m33;
    _34 := m34;

    _41 := m41;
    _42 := m42;
    _43 := m43;
    _44 := m44;

    _51 := m51;
    _52 := m52;
    _53 := m53;
    _54 := m54;
end;

function DWTextRange(start, length: integer) : TDWriteTextRange;
begin
  Result.startPosition := start;
  Result.length := length;
end;

procedure SetInputEffect(effect: ID2D1Effect; index: UINT32;
  inputEffect: ID2D1Effect; invalidate: longbool=TRUE);
var
  output: ID2D1Image;
begin
  output := nil;
  if Assigned(inputEffect) then
    inputEffect.GetOutput(output);
  if Assigned(output) then
    effect.SetInput(index, output, invalidate);
  output := nil;
end;

function D2D1BitmapProperties1(const ABitmapOptions: D2D1_BITMAP_OPTIONS; const APixelFormat: TD2D1PixelFormat;
  const ADpiX: Single = 0; const ADpiY: Single = 0; const AColorContext: ID2D1ColorContext = nil): D2D1_BITMAP_PROPERTIES1;
begin
  Result._pixelFormat := APixelFormat;
  Result.dpiX := ADpiX;
  Result.dpiY := ADpiY;
  Result.bitmapOptions := ABitmapOptions;
  Result.colorContext := AColorContext;
end;

end.
