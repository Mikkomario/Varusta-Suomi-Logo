package xee.varusta.controller

import java.nio.file.Path
import utopia.flow.util.CollectionExtensions._
import utopia.flow.util.FileExtensions._
import utopia.genesis.color.{Color, Rgb}
import utopia.genesis.image.Image
import utopia.genesis.shape.shape2D.Direction2D.Down
import utopia.genesis.shape.shape2D.{Bounds, Line, Point, Size}
import utopia.genesis.util.Drawer
import utopia.reflection.text.{Font, FontMetricsContext, MeasuredText}
import utopia.reflection.localization.{Localizer, NoLocalization}

/**
 * Used for adding text to the main logo
 * @author Mikko Hilpinen
 * @since 9.5.2021, v0.1
 */
object LogoMaker
{
	private implicit val languageCode: String = "fi"
	private implicit val localizer: Localizer = NoLocalization
	
	private val dataDirectory: Path = "data"
	private val imagesDirectory = dataDirectory/"images"
	private val outputDirectory: Path = "output"
	
	private val smallMargin = 64
	private val mediumMargin = 96
	private val largeMargin = 192
	
	private val white = Color.white
	private val blue = Rgb.withValues(0, 47, 108)
	
	/**
	 * Creates logos for the specified names
	 * @param names Names for which logos are created
	 * @param font Font to use in the logos
	 * @return Success or failure
	 */
	def apply(names: Iterable[String], font: Font) =
	{
		// Loads the base images
		Image.readFrom(imagesDirectory/"fire-normal.png").flatMap { normalFire =>
			Image.readFrom(imagesDirectory/"fire-dark.png").flatMap { darkFire =>
				// The text is separated into two lines "Varusta" + "Suomi" and a two-line exclamation point.
				// The text needs to have the same width as the image
				names.tryForeach { name =>
					makeImage(name, normalFire, white, blue, font)
						.writeToFile(outputDirectory/s"varusta-${name.toLowerCase}-light.png")
						.flatMap { _ =>
							makeImage(name, darkFire, blue, white, font)
								.writeToFile(outputDirectory/s"varusta-${name.toLowerCase}-dark.png")
						}
				}
			}
		}
	}
	
	private def makeImage(name: String, fireImage: Image, background: Color, textColor: Color, font: Font) =
	{
		val mainWidth = fireImage.width
		val totalWidth = mainWidth + largeMargin * 2
		val startingHeight = fireImage.height + fireImage.width + largeMargin * 2
		var correctHeight = 0.0 // This will get updated during the draw operation
		val baseImage = Image.paint(Size(totalWidth, startingHeight)) { drawer =>
			// Measuring the natural text size
			val textDrawer = drawer.onlyEdges(textColor)
			val fontMetricsContext = FontMetricsContext(textDrawer.fontMetricsWith(font.toAwt), smallMargin)
			
			val varustaText = new CroppedText(MeasuredText("VARUSTA", fontMetricsContext), font, textDrawer)
			val nameText = new CroppedText(MeasuredText(name, fontMetricsContext), font, textDrawer)
			val exclamationText = new CroppedText(MeasuredText("!", fontMetricsContext), font, textDrawer)
			
			val relativeNameTextScaling = varustaText.width / nameText.width
			// println(s"$name to varusta text ratio = $relativeNameTextScaling")
			// Calculating how scaling affects the sizes
			def exclamationScalingPerMainScaling(mainScaling: Double) =
			{
				val targetHeight = varustaText.height * mainScaling + smallMargin +
					nameText.height * mainScaling * relativeNameTextScaling
				 targetHeight / exclamationText.height
			}
			def widthPerMainScaling(mainScaling: Double) =
			{
				varustaText.width * mainScaling + smallMargin +
					exclamationText.width * exclamationScalingPerMainScaling(mainScaling)
			}
			
			// println(s"Scaling 1.0: ${widthPerMainScaling(1.0)} px")
			// println(s"Scaling 2.0: ${widthPerMainScaling(2.0)} px")
			// println(s"Scaling 3.0: ${widthPerMainScaling(3.0)} px")
			
			// Creating a line that represents the area width (y) based on main scaling (x)
			val widthLine = Line(Point(1.0, widthPerMainScaling(1.0)),
				Point(2.0, widthPerMainScaling(2.0)))
			// Checks at which main scaling (x) the target width is reached
			val correctMainScaling = widthLine.xForY(mainWidth)
			// println(s"Correct main scaling = $correctMainScaling")
			// println(s"Target width = $mainWidth. Width with correct scaling = ${widthPerMainScaling(correctMainScaling)}")
			val textStartX = largeMargin
			val varustaY = fireImage.height + mediumMargin * 2
			val nameY = varustaY + correctMainScaling * varustaText.height + smallMargin
			val exclamationX = textStartX + varustaText.width * correctMainScaling + smallMargin
			correctHeight = nameY + correctMainScaling * relativeNameTextScaling * nameText.height + mediumMargin
			
			// println(s"Varusta size = ${varustaText.size * correctMainScaling}")
			// println(s"$name size = ${nameText.size * correctMainScaling * relativeNameTextScaling}")
			// println(s"! size = ${exclamationText.size * exclamationScalingPerMainScaling(correctMainScaling)}")
			
			// Paints the image (background + fire + texts)
			drawer.onlyFill(background).draw(Bounds(Point.origin, Size(totalWidth, correctHeight)))
			
			/*
			val testDrawer = drawer.onlyFill(Color.magenta)
			testDrawer.draw(Bounds(Point(textStartX, varustaY), varustaText.size * correctMainScaling))
			testDrawer.draw(Bounds(Point(textStartX, nameY),
				nameText.size * correctMainScaling * relativeNameTextScaling))
			testDrawer.draw(Bounds(Point(exclamationX, varustaY),
				exclamationText.size * exclamationScalingPerMainScaling(correctMainScaling)))*/
			
			drawer.drawImage(fireImage, Point(textStartX, mediumMargin))
			
			varustaText.draw(Point(textStartX, varustaY), correctMainScaling)
			nameText.draw(Point(textStartX, nameY), correctMainScaling * relativeNameTextScaling)
			exclamationText.draw(Point(exclamationX, varustaY), exclamationScalingPerMainScaling(correctMainScaling))
		}
		// Crops the base image to correct height
		baseImage.cropFromSide(Down, baseImage.height - correctHeight)
	}
	
	
	// NESTED   ----------------------------------
	
	private class CroppedText(original: MeasuredText, font: Font, drawer: Drawer)
	{
		// ATTRIBUTES   -----------------------
		
		private val relativeBounds =
		{
			val context = drawer.graphics.getFontRenderContext
			val vector = font.toAwt.createGlyphVector(context, original.text.string)
			Bounds.fromAwt(vector.getPixelBounds(null, 0, 0))
		}
		
		
		// COMPUTED ---------------------------
		
		// def size = relativeBounds.size
		def width = relativeBounds.width
		def height = relativeBounds.height
		
		
		// OTHER    ----------------------------
		
		def draw(topLeft: Point, scaling: Double) =
		{
			// println(relativeBounds * scaling)
			// TODO: Currently this draws to wrong position because ascend is calculated twice
			//  Used a temporary workaround by modifying Drawer.drawText method
			val offset = relativeBounds.position * -scaling
			drawer.drawText(original.text.string, (font * scaling).toAwt, topLeft + offset)
		}
	}
}
