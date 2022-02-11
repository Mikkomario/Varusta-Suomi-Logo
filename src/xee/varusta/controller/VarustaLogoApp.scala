package xee.varusta.controller

import utopia.flow.util.FileExtensions._
import utopia.genesis.generic.GenesisDataType
import utopia.reflection.text.Font

/**
 * The main application of this project
 * @author Mikko Hilpinen
 * @since 9.5.2021, v0.1
 */
object VarustaLogoApp extends App
{
	GenesisDataType.setup()
	
	// Loads the font from a file (may throw)
	val font = Font.load("data/fonts/Damavand-Demibold.ttf", 100).get
	
	val names = Vector("FORSSA", "KESKI-SUOMI", "JYVÄSKYLÄ", "PORI", "KYMI", "KOKKOLA", "KAINUU")
	println(s"Generating logos for ${names.mkString(", ")}")
	LogoMaker(names, font)
	println("Logo generation completed!")
}
