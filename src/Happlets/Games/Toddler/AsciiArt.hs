module Happlets.Games.Toddler.AsciiArt where

import           Happlets.Lib.Gtk
import           Happlets.Draw.Text
                   ( TextGridRow(..), TextGridColumn(..), gridColumn, columnInt,
                     TextGridLocation(..), TextGridSize, textGridLocation
                   )

import           Data.Bits
import           Data.Semigroup
import qualified Data.Vector.Unboxed.Mutable as Mutable
import qualified Data.Text                   as Strict
import           Data.Word

import           Linear.V2

import           System.IO

import qualified Graphics.Rendering.Cairo    as Cairo

----------------------------------------------------------------------------------------------------

-- | The game has a fixed-size text matrix with 512 x 256 cells. The window starts at the top-left
-- of the matrix. You can move the window around within the matrix by holding control and pressing
-- the arrow keys, and you can move a cursor around within the window by pressing the arrow
-- keys. You populate the matrix with colored glyphs by typing keys on the keyboard. You can switch
-- colors by pressing the F-1 through F-12 keys. Shift F-1 through Shift F-12 to switch the
-- background color of the glyph.
--
-- Future versions of the game may allow for construction of simple animation and playback by
-- including an event history of all text input at which locations, and including a keyframe command
-- like Ctrl-Return for "next frame" Ctrl-Backspace for "previous frame", and Ctrl-Space for "play
-- animation".
--
-- Future versions of the game may allow for line drawing, using shift arrow-keys to start a line
-- cursor, and then pressing a key to fill the line with characters.
data AsciiArtGame
  = AsciiArtGame
    { theAsciiForecolor :: !GameColor
    , theAsciiBackcolor :: !GameColor
    , theAsciiCursor    :: !TextGridLocation
    , theAsciiWinSize   :: !TextGridSize
      -- ^ the size of the game's visible window, in grid size units.
    , theAsciiWinOff    :: !TextGridLocation
      -- ^ the window offset relative to the top-left of the text matrix.
    , theAsciiFontSize  :: !Double
    , theAsciiMatrix    :: !(Mutable.IOVector Word32)
    }

newAsciiArtGame :: IO AsciiArtGame
newAsciiArtGame = do
  let (TextGridLocation (TextGridRow rows) (TextGridColumn columns)) = theAsciiMatrixSize
  vec <- Mutable.replicate (rows * columns)
      (fromIntegral (ord ' ') .|. gameForecolorToCell WHITE .|. gameBackcolorToCell BLACK)
  return AsciiArtGame
    { theAsciiForecolor = WHITE
    , theAsciiBackcolor = BLACK
    , theAsciiCursor    = textGridLocation
    , theAsciiWinOff    = textGridLocation
    , theAsciiWinSize   = textGridLocation
    , theAsciiFontSize  = 40.0
    , theAsciiMatrix    = vec
    }

startAsciiArtGame :: PixSize -> GtkGUI AsciiArtGame ()
startAsciiArtGame winsize = do
  updateWindowGridSize winsize
  resizeEvents redrawWindow
  keyboardEvents $ setGameColor <> fillCell <> moveCursor
  mouseEvents MouseButton $ \ case
    (Mouse _ True mod LeftClick coord) | mod == noModifiers ->
      mouseToGrid coord >>= assign asciiCursor
    _ -> return ()
  bg <- use asciiBackcolor
  let (r, g, b, _) = unpackRGBA32Color $ dark 0.75 $ gameColor bg
  onCanvas $ cairoRender $ cairoClearCanvas r g b 0.75

----------------------------------------------------------------------------------------------------

-- | There are 12 game colors. The plan is to make colors selectable by the F-1 through F-12 keys on
-- the keyboard.
data GameColor
  = BLACK | GRAY | WHITE | RED | ORANGE | YELLOW | GREEN | CYAN | BLUE | VIOLET | MAGENTA | BROWN
  deriving (Eq, Ord, Show, Read, Enum)

gameColor :: GameColor -> Color
gameColor = \ case
  RED     -> packRGBA32Color 1.0  0.0 0.0 1.0
  ORANGE  -> packRGBA32Color 1.0  0.5 0.0 1.0
  YELLOW  -> packRGBA32Color 1.0  1.0 0.0 1.0
  GREEN   -> packRGBA32Color 0.0  0.5 0.0 1.0
  CYAN    -> packRGBA32Color 0.0  1.0 1.0 1.0
  BLUE    -> packRGBA32Color 0.0  0.0 1.0 1.0
  VIOLET  -> packRGBA32Color 0.5  0.0 1.0 1.0
  MAGENTA -> packRGBA32Color 1.0  0.0 1.0 1.0
  BROWN   -> packRGBA32Color 0.5 0.25 0.0 1.0
  WHITE   -> packRGBA32Color 1.0  1.0 1.0 1.0
  GRAY    -> packRGBA32Color 0.5  0.5 0.5 1.0
  BLACK   -> packRGBA32Color 0.0  0.0 0.0 1.0

-- | Takes a keyboard event. If the keyboard event doesn't match a key for setting the color, this
-- function does nothing.
setGameColor :: Keyboard -> GtkGUI AsciiArtGame ()
setGameColor = \ case
  Keyboard pressed mods point -> when pressed $ do
    let color = if shiftIsSet mods then asciiBackcolor else asciiForecolor
    case point of
      FuncKey  1 -> color .= RED
      FuncKey  2 -> color .= ORANGE
      FuncKey  3 -> color .= YELLOW
      FuncKey  4 -> color .= GREEN
      FuncKey  5 -> color .= CYAN
      FuncKey  6 -> color .= BLUE
      FuncKey  7 -> color .= VIOLET
      FuncKey  8 -> color .= MAGENTA
      FuncKey  9 -> color .= BROWN
      FuncKey 10 -> color .= WHITE
      FuncKey 11 -> color .= GRAY
      FuncKey 12 -> color .= BLACK
      _          -> return ()
  RawKey   pressed mods point -> liftIO $ hPutStrLn stderr $
    (if pressed then "[raw key DOWN] " else "[raw  key  UP] ") ++ show point ++ ' ' : show mods

-- | Write to a given cell location with a given color.
writeToCellWithColor
  :: TextGridLocation
  -> GameColor
  -> GameColor
  -> Char
  -> GtkGUI AsciiArtGame ()
writeToCellWithColor loc fg bg c = do
  ( Mutable.write <$> use asciiMatrix <*> locationToIndex loc <*>
      pure (fromIntegral (ord c) .|. gameForecolorToCell fg .|. gameBackcolorToCell bg)
    ) >>= liftIO
  use asciiCursor >>= redrawAtPosition . pure

-- | Write to a cell with the current color at a different location.
writeToCellAt :: TextGridLocation -> Char -> GtkGUI AsciiArtGame ()
writeToCellAt loc c = join $ 
  writeToCellWithColor loc <$> use asciiForecolor <*> use asciiBackcolor <*> pure c

-- | Write to a cell at the current cursor position with the current color.
writeToCell :: Char -> GtkGUI AsciiArtGame ()
writeToCell c = use asciiCursor >>= \ loc -> writeToCellAt loc c

fillCell :: Keyboard -> GtkGUI AsciiArtGame ()
fillCell = \ case
  Keyboard pressed mods point -> 
    when (pressed && not (altIsSet mods || ctrlIsSet mods || superIsSet mods)) $ case point of
      CharKey c -> writeToCell c >> advanceCursor (TextGridRow 0) (TextGridColumn 1)
      _         -> return ()
  _                           -> return ()

moveCursor :: Keyboard -> GtkGUI AsciiArtGame ()
moveCursor = \ case
  Keyboard pressed mods point -> when (pressed && mods == noModifiers) $ case point of
    UpArrowKey    -> advanceCursor (TextGridRow (-1)) (TextGridColumn   0 )
    DownArrowKey  -> advanceCursor (TextGridRow   1 ) (TextGridColumn   0 )
    LeftArrowKey  -> advanceCursor (TextGridRow   0 ) (TextGridColumn (-1))
    RightArrowKey -> advanceCursor (TextGridRow   0 ) (TextGridColumn   1 )
    EnterKey      -> enterKey
    ReturnKey     -> enterKey
    BackSpaceKey  -> advanceCursor (TextGridRow   0 ) (TextGridColumn (-1)) >> writeToCell ' '
    _             -> return ()
  _             -> return ()
  where
    enterKey = do
      advanceCursor (TextGridRow   1 ) (TextGridColumn   0 )
      asciiCursor . gridColumn . columnInt .= 0

-- | The foreground color take up the first 4 bits in the upper 11 bits of the Word32 after the
-- first 21 reserved for the character.
gameForecolorToCell :: GameColor -> Word32
gameForecolorToCell = (`shift` 22) . fromIntegral . fromEnum

-- | The foreground color take up the first 4 bits in the upper 11 bits of the Word32 after the
-- first 21 reserved for the character, and the next 4 used for the foreground color.
gameBackcolorToCell :: GameColor -> Word32
gameBackcolorToCell = (`shift` 26) . fromIntegral . fromEnum

gameCellToBackcolor :: Word32 -> GameColor
gameCellToBackcolor = toEnum . fromIntegral . (0x0F .&.) . (`shift` (-26))

gameCellToForecolor :: Word32 -> GameColor
gameCellToForecolor = toEnum . fromIntegral . (0x0F .&.) . (`shift` (-22))

-- | The matrix size is fixed at 512 columns, 256 rows of text, so we don't have to do a bunch of
-- resizing of rows and colums. The memory taken for the buffer is
-- @256*512*('Data.Bits.bitSize' 0 :: 'Data.Word.Word32') == 2^8 * 2^9 * 2^2 == 2^19@
-- which is 524,288 bytes, or half of a megabyte.
theAsciiMatrixSize :: TextGridSize
theAsciiMatrixSize = TextGridLocation (TextGridRow 256) (TextGridColumn 512)

asciiForecolor  :: Lens' AsciiArtGame GameColor
asciiForecolor  = lens theAsciiForecolor $ \ a b -> a{ theAsciiForecolor = b }

asciiBackcolor  :: Lens' AsciiArtGame GameColor
asciiBackcolor  = lens theAsciiBackcolor $ \ a b -> a{ theAsciiBackcolor = b }

asciiCursor     :: Lens' AsciiArtGame TextGridLocation
asciiCursor     = lens theAsciiCursor $ \ a b -> a{ theAsciiCursor = b }

asciiWinSize    :: Lens' AsciiArtGame TextGridSize
asciiWinSize    = lens theAsciiWinSize $ \ a b -> a{ theAsciiWinSize = b }

asciiWinOff     :: Lens' AsciiArtGame TextGridLocation
asciiWinOff     = lens theAsciiWinOff $ \ a b -> a{ theAsciiWinOff = b }

asciiMatrix     :: Lens' AsciiArtGame (Mutable.IOVector Word32)
asciiMatrix     = lens theAsciiMatrix $ \ a b -> a{ theAsciiMatrix = b }

asciiFontSize   :: Lens' AsciiArtGame Double
asciiFontSize   = lens theAsciiFontSize $ \ a b -> a{ theAsciiFontSize = b }

redrawWindow :: PixSize -> GtkGUI AsciiArtGame ()
redrawWindow siz = updateWindowGridSize siz >> redrawAll

redrawAll :: GtkGUI AsciiArtGame ()
redrawAll = do
  (TextGridLocation (TextGridRow lowRow) (TextGridColumn lowCol)) <- use asciiWinOff
  (TextGridLocation (TextGridRow rows  ) (TextGridColumn cols  )) <- use asciiWinSize
  redrawAtPosition $ TextGridLocation
    <$> (TextGridRow    <$> [lowRow .. lowRow + rows])
    <*> (TextGridColumn <$> [lowCol .. lowCol + cols])

-- | Find the pixel region covered by the current relative cursor position and redraw it on screen.
redrawAtPosition :: [TextGridLocation] -> GtkGUI AsciiArtGame ()
redrawAtPosition points = do
  (TextGridLocation (TextGridRow winOffRow) (TextGridColumn winOffCol)) <- use asciiWinOff
  fontSize <- use asciiFontSize
  word <- Mutable.read <$> use asciiMatrix <*> cursorToIndex >>= liftIO
  onCanvas $ cairoRender $ do
    -- Get font extents first, all drawing operations are computed from these values.
    Cairo.selectFontFace ("monospace" :: Strict.Text) Cairo.FontSlantNormal Cairo.FontWeightNormal
    Cairo.setFontSize fontSize
    ext <- Cairo.fontExtents
    --let ascent     = Cairo.fontExtentsAscent ext
    let descent    = Cairo.fontExtentsDescent ext
    let fontHeight = max (Cairo.fontExtentsMaxYadvance ext) fontSize
    let fontWidth  = max (Cairo.fontExtentsMaxXadvance ext) (fontHeight / 2.0)
    forM_ points $ \ (TextGridLocation (TextGridRow curRow) (TextGridColumn curCol)) -> do
      -- Draw background
      let (r, g, b, _) = unpackRGBA32Color $ gameColor $ gameCellToBackcolor word
      Cairo.setSourceRGBA r g b 0.75
      op <- Cairo.getOperator
      Cairo.setOperator Cairo.OperatorSource
      let row = curRow - winOffRow
      let col = curCol - winOffCol
      Cairo.rectangle
        (realToFrac col * fontWidth + 0.5) (realToFrac row * (fontHeight + descent) + 0.5)
        (fontWidth + 0.5) (fontHeight + descent + 0.5)
      Cairo.fill
      Cairo.setOperator op
      -- Draw text
      cairoSetColor $ gameColor $ gameCellToForecolor word
      Cairo.moveTo
        (realToFrac col * fontWidth + 0.5)
        (realToFrac (row+1) * (fontHeight + descent) + 0.5 - descent)
      Cairo.showText [chr $ fromIntegral $ word .&. 0x003FFFFF]

-- | Get the current size of the window in units of grid cells. This depends on the font size.
pixPointToGrid :: PixCoord -> GtkGUI AsciiArtGame TextGridLocation
pixPointToGrid (V2 x y) = do
  fontSize <- use asciiFontSize
  ext <- onOSBuffer $ cairoRender $ do
    Cairo.selectFontFace ("monospace" :: Strict.Text) Cairo.FontSlantNormal Cairo.FontWeightNormal
    Cairo.setFontSize fontSize
    Cairo.fontExtents
  let fontHeight = max (Cairo.fontExtentsMaxYadvance ext) fontSize
  let fontWidth  = max (Cairo.fontExtentsMaxXadvance ext) (fontHeight / 2.0)
  let descent    = Cairo.fontExtentsDescent ext
  return $ TextGridLocation
    (TextGridRow    $ round (realToFrac y / (fontHeight + descent)))
    (TextGridColumn $ round (realToFrac x / fontWidth ))

-- | Convert a mouse position (window local coordinates) to a global 'asciiMatrix' location.
mouseToGrid :: PixCoord -> GtkGUI AsciiArtGame TextGridLocation
mouseToGrid = pixPointToGrid >=> \ (TextGridLocation (TextGridRow row) (TextGridColumn col)) -> do
  (TextGridLocation (TextGridRow offRow) (TextGridColumn offCol)) <- use asciiWinOff
  return $ TextGridLocation
    (TextGridRow    $ row + offRow)
    (TextGridColumn $ col + offCol)

-- | Sets the 'asciiWinSize' to the result of 'pixPointToGrid', caching the grid size for other
-- computations to use. This function must be called whenever the size of the app window changes.
updateWindowGridSize :: PixSize -> GtkGUI AsciiArtGame ()
updateWindowGridSize = pixPointToGrid >=> assign asciiWinSize

-- | Retrieve the current cursor position relative to the window offset so you know where it should
-- be seen in the window.
getRelativeCursor :: GtkGUI AsciiArtGame TextGridLocation
getRelativeCursor = do
  (TextGridLocation (TextGridRow globalRow) (TextGridColumn globalCol)) <- use asciiCursor
  (TextGridLocation (TextGridRow winOffRow) (TextGridColumn winOffCol)) <- use asciiWinOff
  return $ TextGridLocation
    (TextGridRow    $ globalRow - winOffRow)
    (TextGridColumn $ globalCol - winOffCol)

-- | Convert a 'TextGridLocation' to an index in the 'asciiMatrix'. Locations that are out of bounds
-- are "wrapped around" using the 'Prelude.mod' operator to ensure they are always in bounds, so
-- this function never throws an exception.
locationToIndex :: TextGridLocation -> GtkGUI AsciiArtGame Int
locationToIndex (TextGridLocation (TextGridRow row) (TextGridColumn col)) = do
  let (TextGridLocation (TextGridRow rowsize) (TextGridColumn colsize)) = theAsciiMatrixSize
  return $ mod row rowsize * colsize + mod col colsize

-- | Convert the cursor to a position to an index in the 'asciiMatrix'.
cursorToIndex :: GtkGUI AsciiArtGame Int
cursorToIndex = use asciiCursor >>= locationToIndex

-- | Advance the cursor down by @n@ rows and forward by @n@ columns. The cursor is wrapped to the
-- window without modifying the window's offset in the text matrix, allowing the cursor to be
-- wrapped more easily, as opposed to only wrapping when the end of the matrix is reached.
advanceCursor :: TextGridRow -> TextGridColumn -> GtkGUI AsciiArtGame ()
advanceCursor (TextGridRow deltaRow) (TextGridColumn deltaCol) = do
  (TextGridLocation (TextGridRow winRowSize) (TextGridColumn winColSize)) <- use asciiWinSize
  (TextGridLocation (TextGridRow curRow) (TextGridColumn curCol)) <- getRelativeCursor
  let offset = (curRow + deltaRow) * winColSize + curCol + deltaCol
  let (newRow, newCol) = divMod offset winColSize
  asciiCursor .= TextGridLocation (TextGridRow $ mod newRow winRowSize) (TextGridColumn $ newCol)
