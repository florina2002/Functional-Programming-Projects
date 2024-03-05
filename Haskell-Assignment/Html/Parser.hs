module Html.Parser where

import Html
import qualified Parser as P
import Parser (satisfies)
import Parser (orElse)
import Parser (char)



import Result

-- | Parses HTML text, consuming input as long as it doesn't contain the characters < or >
--
-- >>> P.parse text "some text"
-- Success "some text"
--
-- >>> P.parse text "!@#$%^&&*()"
-- Success "!@#$%^&&*()"
--
-- >>> P.runParser text "some text<"
-- Success ("some text","<")
--
-- >>> P.runParser text "<some text>"
-- Error (UnexpectedInput {gotInput = "<some text>", expectedInput = "At least one character"})
-- | Parses HTML text, consuming input as long as it doesn't contain the characters < or >

-- text :: P.Parser String
-- text = P.many (P.satisfies (`notElem` ['<', '>']) "not < or >")

text :: P.Parser String
text = nonEmptyText `P.orElse` failOnEmptyOrAngleBracket
  where
    nonEmptyText = P.some (P.satisfies (`notElem` ['<', '>']) "not < or >")

    failOnEmptyOrAngleBracket = P.parser parse
      where
        parse "" = Result.Error P.UnexpectedEndOfInput
        parse (x:xs)
          | x == '<' || x == '>' = Result.Error (P.UnexpectedInput [x] "not < or >")
          | otherwise = Result.Success ("", x:xs)




-- | Parses a self closing tag
--
-- A self closing tag is an identifier followed by a possibly empty list of attributes (see @attributes@) between < and />
--
-- Some useful functions:
-- - @Parser.ident@
-- - @Parser.between@
-- - @Parser.tag@
-- - @Parser.char@
--
-- >>> P.parse selfClosing "<a/>"
-- Success ("a",[])
--
-- >>> P.parse selfClosing "<a>"
-- Error (UnexpectedInput {gotInput = ">", expectedInput = "character '/'"})
--
-- >>> P.parse selfClosing "<a x=\"y\"/>"
-- Success ("a",[("x",Just "y")])
--
-- >>> P.parse selfClosing "<a x/>"
-- Success ("a",[("x",Nothing)])
selfClosing :: P.Parser (String, [(String, Maybe String)])
selfClosing = error "selfClosing"

-- | Parses an opening tag
--
-- A tag is an identifier followed by a possibly empty list of attributes (see @attributes@) between < and >
--
-- Note: There might be whitespace between the identifier and >
--
-- Some useful functions:
-- - @Parser.between@
-- - @Parser.tag@
-- - @Parser.char@
--
-- >>> P.parse openTag "<a>"
-- Success ("a",[])
--
-- >>> P.parse openTag "<a >"
-- Success ("a",[])
openTag :: P.Parser (String, [(String, Maybe String)])
openTag = error "openTag"

-- | Parses a possibly empty list of attributes
--
-- An attribute is a key, optionally followed by = and a value between double quotes or single qoutes.
-- The attributes are separated by whitespace.
--
-- Some useful functions:
-- - @Parser.sepBy@
-- - @Parser.between@
-- - @Parser.opt@
-- - @Parser.orElse@
--
-- >>> P.parse attributes "a=\"b\""
-- Success [("a",Just "b")]
--
-- >>> P.parse attributes "a='b'"
-- Success [("a",Just "b")]
--
-- >>> P.parse attributes "a=\"\""
-- Success [("a",Just "")]
--
-- >>> P.parse attributes "a b=\"x\""
-- Success [("a",Nothing),("b",Just "x")]
--
-- >>> P.parse attributes "a b"
-- Success [("a",Nothing),("b",Nothing)]
attributes :: P.Parser [(String, Maybe String)]
attributes = error "attributes"


-- | Parses the given closing tag
--
-- A closing tag is an identifier between </ and >
--
-- Note: There might be whitespace between the identifier and >
--
-- Some useful functions:
-- - @Parser.tag@
--
-- >>> P.parse (closingTag "a") "</a>"
-- Success ()
--
-- >>> P.parse (closingTag "a") "</a  >"
-- Success ()
--
-- >>> P.parse (closingTag "a") "</div>"
-- Error (UnexpectedInput {gotInput = "d", expectedInput = "character 'a'"})
closingTag :: String -> P.Parser ()
closingTag tag = error "closingTag"

-- | Run a parser between HTML tags, checking that the opening and closing tag match.
--
-- Some useful functions:
-- - @openTag@
-- - @closingTag@
-- - @Parser.pWith@
-- 
-- >>> P.parse (betweenHtmlTags (P.char 'x')) "<a>x</a>"
-- Success ('x',"a",[])
--
-- >>> P.parse (betweenHtmlTags (P.char 'x')) "<a>x</b>"
-- Error (UnexpectedInput {gotInput = "b", expectedInput = "character 'a'"})
--
-- >>> P.parse (betweenHtmlTags (P.char 'x')) "<a y=\"z\">x</a>"
-- Success ('x',"a",[("y",Just "z")])
--
-- >>> P.parse (betweenHtmlTags (P.char 'x')) "<a y>x</a>"
-- Success ('x',"a",[("y",Nothing)])
betweenHtmlTags :: P.Parser a -> P.Parser (a, String, [(String, Maybe String)])
betweenHtmlTags p = error "betweenHtmlTags"

-- | Parses a HTML node
--
-- A HTML node is one of:
-- - a self closing tag
-- - an opening and closing tag, with more HTML between them
--
-- Some useful functions:
-- - @Parser.pMap@
-- - @Parser.orElse@
-- - @html@
-- - @betweenHtmlTags@
-- - @selfClosing@
--
-- >>> P.parse htmlNode "<a/>"
-- Success (HtmlNode {nodeTag = "a", nodeAttrs = [], nodeChildren = []})
--
-- >>> P.parse htmlNode "<a>b</a>"
-- Success (HtmlNode {nodeTag = "a", nodeAttrs = [], nodeChildren = [b]})
--
-- >>> P.parse htmlNode "<a><b>text</b></a>"
-- Success (HtmlNode {nodeTag = "a", nodeAttrs = [], nodeChildren = [<b>
--   text
-- </b>]})
htmlNode :: P.Parser HtmlNode
htmlNode = error "htmlNode"

-- | Parses a HTML node or a text node
--
-- Some useful functions:
-- - @Parser.pMap@
-- - @Parser.orElse@
-- - @htmlNode@
-- - @text@
--
-- >>> P.parse html "<a y=\"z\"><b>x</b></a>"
-- Success <a y="z">
--   <b>
--     x
--   </b>
-- </a>
--
-- >>> P.parse html "<a y=\"z\"><b>x <z></z></b></a>"
-- Success <a y="z">
--   <b>
--     x
--     <z/>
--   </b>
-- </a>
html :: P.Parser Html
html = error "html"

document :: P.Parser Document
document = P.eof $ P.pMap Document $ P.some html

parse :: String -> Result P.ParseError Document
parse = P.parse document

-- >>> parseNode "<div></div>"
-- Success (HtmlNode {nodeTag = "div", nodeAttrs = [], nodeChildren = []})
--
-- >>> parseNode "<div></div><p></p>"
-- Error (ExpectedEOF {remainingInput = "<p></p>"})
parseNode :: String -> Result P.ParseError HtmlNode
parseNode = P.parse (P.eof htmlNode)
