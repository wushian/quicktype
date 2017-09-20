module Language.Swift
    ( swift3Renderer
    , swift4Renderer
    ) where

import Prelude

import Data.Array as A
import Data.Char.Unicode (isAlphaNum, isDigit)
import Data.Foldable (for_, null)
import Data.List as L
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Util (camelCase, capitalize, decapitalize, genericStringEscape, intToHex, legalizeCharacters, startWithLetter)
import Data.Tuple (Tuple(..))
import Doc (Doc, Namer, Renderer, blank, combineNames, forEachClass_, forEachProperty_, forEachTopLevel_, forEachUnion_, forbidNamer, getTypeNameForUnion, indent, line, lookupClassName, lookupName, lookupUnionName, renderRenderItems, simpleNamer, transformPropertyNames, unionIsNotSimpleNullable, unionNameIntercalated)
import IRGraph (IRClassData(..), IRType(..), IRUnionRep, canBeNull, forUnion_, isUnionMember, nullableFromUnion, removeNullFromUnion, unionToList)

keywords :: Array String
keywords =
    [ "associatedtype", "class", "deinit", "enum", "extension", "fileprivate", "func", "import", "init", "inout", "internal", "let", "open", "operator", "private", "protocol", "public", "static", "struct", "subscript", "typealias", "var"
    , "break", "case", "continue", "default", "defer", "do", "else", "fallthrough", "for", "guard", "if", "in", "repeat", "return", "switch", "where", "while"
    , "as", "Any", "catch", "false", "is", "nil", "rethrows", "super", "self", "Self", "throw", "throws", "true", "try"
    , "_"
    , "associativity", "convenience", "dynamic", "didSet", "final", "get", "infix", "indirect", "lazy", "left", "mutating", "nonmutating", "optional", "override", "postfix", "precedence", "prefix", "Protocol", "required", "right", "set", "Type", "unowned", "weak", "willSet"
    , "String", "Int", "Double", "Bool", "Data", "CommandLine", "FileHandle", "JSONSerialization"
    , "checkNull", "removeNSNull", "nilToNSNull", "convertArray", "convertOptional", "convertDict", "convertDouble"
    ]

swift3Renderer :: Renderer
swift3Renderer =
    { displayName: "Swift 3"
    , names: [ "swift3" ]
    , aceMode: "swift"
    , extension: "swift"
    , doc: swift3Doc
    , options: []
    , transforms:
        { nameForClass: simpleNamer nameForClass
        , nextName: \s -> "Other" <> s
        , forbiddenNames: keywords
        , topLevelName: forbidNamer (swiftNameStyle true) (\n -> [swiftNameStyle true n])
        , unions: Just
            { predicate: unionIsNotSimpleNullable
            , properName: simpleNamer (swiftNameStyle true <<< combineNames)
            , nameFromTypes: simpleNamer (unionNameIntercalated (swiftNameStyle true) "Or")
            }
        }
    }

swift4Renderer :: Renderer
swift4Renderer =
    { displayName: "Swift 4"
    , names: [ "swift4" ]
    , aceMode: "swift"
    , extension: "swift"
    , doc: swift4Doc
    , options: []
    , transforms:
        { nameForClass: simpleNamer nameForClass
        , nextName: \s -> "Other" <> s
        , forbiddenNames: keywords
        , topLevelName: forbidNamer (swiftNameStyle true) (\n -> [swiftNameStyle true n])
        , unions: Just
            { predicate: unionIsNotSimpleNullable
            , properName: simpleNamer (swiftNameStyle true <<< combineNames)
            , nameFromTypes: simpleNamer (unionNameIntercalated (swiftNameStyle true) "Or")
            }
        }
    }

legalize :: String -> String
legalize = legalizeCharacters isPartCharacter
    where
        isPartCharacter :: Char -> Boolean
        isPartCharacter c = c == '_' || isAlphaNum c

swiftNameStyle :: Boolean -> String -> String
swiftNameStyle isUpper =
    legalize >>> camelCase >>> startWithLetter isStartCharacter isUpper
    where
        isStartCharacter :: Char -> Boolean
        isStartCharacter c = c == '_' || (isAlphaNum c && not (isDigit c))

nameForClass :: IRClassData -> String
nameForClass (IRClassData { names }) = swiftNameStyle true $ combineNames names

stringEscape :: String -> String
stringEscape =
    genericStringEscape unicodeEscape
    where
        unicodeEscape i =
            "\\u{" <> (String.fromCharArray $ intToHex 0 i) <> "}"

swift3Doc :: Doc Unit
swift3Doc = do
    line "// To parse the JSON, add this file to your project and do:"
    line "//"
    forEachTopLevel_ \topLevelName topLevelType -> do
        typ <- renderType topLevelType
        line $ "//   let " <> decapitalize topLevelName <> " = " <> topLevelName <> "(fromString: jsonString)!"
    blank
    line "import Foundation"
    blank

    renderRenderItems blank (Just renderTopLevelAlias) (renderClassDefinition false) (Just $ renderUnionDefinition false)

    blank
    line $ "// Serialization extensions"

    forEachTopLevel_ renderTopLevelExtensions3

    forEachClass_ \className properties -> do
        blank
        renderClassExtension3 className properties

    forEachUnion_ \unionName unionRep -> do
        blank
        renderUnionExtension3 unionName unionRep

    blank
    supportFunctions3

swift4Doc :: Doc Unit
swift4Doc = do
    -- FIXME: usage comments
    line "import Foundation"
    blank

    renderRenderItems blank (Just renderTopLevelAlias) (renderClassDefinition true) (Just $ renderUnionDefinition true)

    blank
    line $ "// Serialization extensions"

    forEachTopLevel_ renderTopLevelExtensions4

    forEachClass_ \className properties -> do
        blank
        renderClassExtension4 className properties

    forEachUnion_ \unionName unionRep -> do
        blank
        renderUnionExtension4 unionName unionRep
    
    blank
    supportFunctions4

supportFunctions3 :: Doc Unit
supportFunctions3 = do
    line """// Helpers

fileprivate func convertArray<T>(_ converter: (Any) -> T?, _ json: Any) -> [T]? {
    guard let jsonArr = json as? [Any] else { return nil }
    var arr = [T]()
    for v in jsonArr {
        if let converted = converter(v) {
            arr.append(converted)
        } else {
            return nil
        }
    }
    return arr
}

fileprivate func convertOptional<T>(_ converter: (Any) -> T?, _ json: Any?) -> T?? {
    guard let v = json else { return .some(nil) }
    return converter(v)
}

fileprivate func convertDict<T>(_ converter: (Any) -> T?, _ json: Any?) -> [String: T]? {
    guard let jsonDict = json as? [String: Any] else { return nil }
    var dict = [String: T]()
    for (k, v) in jsonDict {
        if let converted = converter(v) {
            dict[k] = converted
        } else {
            return nil
        }
    }
    return dict
}

fileprivate func convertToAny<T>(_ dictionary: [String: T], _ converter: (T) -> Any) -> Any {
    var result = [String: Any]()
    for (k, v) in dictionary {
        result[k] = converter(v)
    }
    return result
}

fileprivate func convertDouble(_ v: Any) -> Double? {
    if let w = v as? Double { return w }
    if let w = v as? Int { return Double(w) }
    return nil
}

fileprivate let falseType = NSNumber(value: false).objCType
fileprivate let trueNumber = NSNumber(value: true)
fileprivate let trueType = trueNumber.objCType

fileprivate func convertBool(_ v: Any?) -> Bool? {
    guard let number = v as? NSNumber
    else {
        if let b = v as? Bool {
            return b
        }
        return nil
    }
    
    if number.objCType != falseType && number.objCType != trueType {
        return nil
    }
    return number.isEqual(trueNumber)
}

fileprivate func removeNSNull(_ v: Any?) -> Any? {
    if let w = v {
        if w is NSNull {
            return nil
        }
        return w
    }
    return nil
}

fileprivate func checkNull(_ v: Any?) -> NSNull? {
    if v != nil { return .none }
    return .some(NSNull())
}"""

supportFunctions4 :: Doc Unit
supportFunctions4 = do
    line """// Helpers

class MyNull: Codable {
    public required init(from decoder: Decoder) throws {
        let container = try decoder.singleValueContainer()
        if !container.decodeNil() {
            throw DecodingError.typeMismatch(MyNull.self, DecodingError.Context(codingPath: decoder.codingPath, debugDescription: "Wrong type for MyNull"))
        }
    }
    
    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encodeNil()
    }
}

typealias MyAny = MyNull"""

renderUnion :: IRUnionRep -> Doc String
renderUnion ur =
    case nullableFromUnion ur of
    Just r -> do
        rendered <- renderType r
        pure $ rendered <> "?"
    Nothing -> lookupUnionName ur

renderType :: IRType -> Doc String
renderType = case _ of
    IRNoInformation -> pure "FIXME_THIS_SHOULD_NOT_HAPPEN"
    IRAnyType -> pure "MyAny?"
    IRNull -> pure "MyNull"
    IRInteger -> pure "Int"
    IRDouble -> pure "Double"
    IRBool -> pure "Bool"
    IRString -> pure "String"
    IRArray a -> do
        rendered <- renderType a
        pure $ "[" <> rendered <> "]"
    IRClass i -> lookupClassName i
    IRMap t -> do
        rendered <- renderType t
        pure $ "[String: " <> rendered <> "]"
    IRUnion ur -> renderUnion ur

convertAny :: IRType -> String -> Doc String
convertAny (IRArray a) var = do
    converter <- convertAnyFunc a
    pure $ "convertArray(" <> converter <> ", " <> var <> ")"
convertAny (IRMap m) var = do
    converter <- convertAnyFunc m
    pure $ "convertDict(" <> converter <> ", " <> var <> ")"
convertAny (IRUnion ur) var =
    case nullableFromUnion ur of
    Just t -> do
        converter <- convertAnyFunc t
        pure $ "convertOptional(" <> converter <> ", " <> var <> ")"
    Nothing -> do
        name <- lookupUnionName ur
        pure $ name <> ".fromJson(" <> var <> ")"
convertAny IRAnyType var =
    pure var
convertAny IRBool var =
    pure $ "convertBool(" <> var <> ")"
convertAny IRInteger var =
    pure $ var <> " as? Int"
convertAny IRString var =
    pure $ var <> " as? String"
convertAny t var = do
    converter <- convertAnyFunc t
    pure $ converter <> "(" <> var <> ")"

convertAnyFunc :: IRType -> Doc String
convertAnyFunc = case _ of
    IRClass i -> do
        name <- lookupClassName i
        pure $ name <> ".init(fromAny:)"
    IRUnion ur ->
        case nullableFromUnion ur of
        Just t -> do
            converter <- convertAnyFunc t
            pure $ "{ (json: Any) in convertOptional(" <> converter <> ", json) }"
        Nothing -> do
            name <- lookupUnionName ur
            pure $ name <> ".fromJson"
    IRDouble -> pure "convertDouble"
    IRNull -> pure "checkNull"
    t -> do
        converted <- convertAny t "$0"
        pure $ "{ " <> converted <> " }"

convertToAny :: IRType -> String -> Doc String
convertToAny (IRArray a) var = do
    convertCode <- convertToAny a "$0"
    pure $ var <> ".map({ " <> convertCode <> " }) as Any"
convertToAny (IRMap m) var = do
    convertCode <- convertToAny m "$0"
    pure $ "convertToAny(" <> var <> ", { "<> convertCode <> " })"
convertToAny (IRClass i) var =
    pure $ var <> ".any"
convertToAny (IRUnion ur) var =
    case nullableFromUnion ur of
    Just t -> do
        convertCode <- convertToAny t "$0"
        pure $ var <> ".map({ " <> convertCode  <> " }) ?? NSNull()"
    Nothing ->
        pure $ var <> ".any"
convertToAny IRAnyType var =
    pure $ var <> " ?? NSNull()"
convertToAny IRNull var =
    pure $ "NSNull()"
convertToAny _ var =
    pure $ var <> " as Any"

renderTopLevelAlias :: String -> IRType -> Doc Unit
renderTopLevelAlias topLevelName topLevelType = do
    top <- renderType topLevelType
    line $ "typealias "<> topLevelName <> " = " <> top

codableString :: Boolean -> String
codableString true = " : Codable"
codableString false = ""

renderClassDefinition :: Boolean -> String -> Map String IRType -> Doc Unit
renderClassDefinition codable className properties = do
    let forbidden = keywords <> ["json", "any"]
    -- FIXME: we compute these here, and later again when rendering the extension
    let propertyNames = makePropertyNames properties "" forbidden
    line $ "struct " <> className <> codableString codable <> " {"
    indent do
        forEachProperty_ properties propertyNames \_ ptype fieldName _ -> do
            rendered <- renderType ptype
            line $ "let " <> fieldName <> ": " <> rendered
    line "}"

renderExtensionType :: IRType -> Doc String
renderExtensionType (IRArray t) = ("Array where Element == " <> _) <$> renderType t
renderExtensionType (IRMap t) = ("Dictionary where Key == String, Value == " <> _) <$> renderType t
renderExtensionType t = renderType t

renderTopLevelExtensions3 :: String -> IRType -> Doc Unit
renderTopLevelExtensions3 topLevelName topLevelType = do
    blank

    topLevelRendered <- renderType topLevelType
    extensionType <- renderExtensionType topLevelType

    line $ "extension " <> extensionType <> " {"
    indent do
        line $ "init?(fromString json: String, using encoding: String.Encoding = .utf8) {"
        indent do
            line "guard let data = json.data(using: encoding) else { return nil }"
            line "self.init(fromData: data)"
        line "}"
        blank
        line $ "init?(fromData data: Data) {"
        indent do
            line "guard let json = try? JSONSerialization.jsonObject(with: data, options: []) else { return nil }"
            line "self.init(fromAny: json)"
        line "}"
        
        case topLevelType of
            IRArray _ ->  do
                blank
                line $ "init?(fromAny untyped: Any) {"
                indent do
                    convertCode <- convertAny topLevelType "untyped"
                    line $ "guard let elements = " <> convertCode <> " else { return nil }"
                    line $ "self.init(elements)"
                line "}"
            IRMap _ -> do
                blank
                line $ "init?(fromAny untyped: Any) {"
                indent do
                    convertCode <- convertAny topLevelType "untyped"
                    line $ "guard let elements = " <> convertCode <> " else { return nil }"
                    line $ "self.init()"
                    line $ "elements.forEach { self[$0.key] = $0.value }"
                line "}"
            _ -> pure unit

        blank
        line $ "var jsonData: Data? {"
        indent do
            convertCode <- convertToAny topLevelType "self"
            line $ "let json = " <> convertCode
            line "return try? JSONSerialization.data(withJSONObject: json, options: [])"
        line "}"
            
        blank
        line $ "var jsonString: String? {"
        indent do
            line $ "guard let data = self.jsonData else { return nil }"
            line $ "return String(data: data, encoding: .utf8)"
        line "}"

    line "}"

renderTopLevelExtensions4 :: String -> IRType -> Doc Unit
renderTopLevelExtensions4 topLevelName topLevelType = do
    blank

    topLevelRendered <- renderType topLevelType
    extensionType <- renderExtensionType topLevelType

    line $ "extension " <> extensionType <> " {"
    indent do
        line $ "init?(fromString json: String, using encoding: String.Encoding = .utf8) {"
        indent do
            line "guard let data = json.data(using: encoding) else { return nil }"
            line "self.init(fromData: data)"
        line "}"
        blank
        line $ "init?(fromData data: Data) {"
        indent do
            line "let decoder = JSONDecoder()"
            line $ "guard let result = try? decoder.decode(" <> topLevelRendered <> ".self, from: data) else { return nil }"
            line "self = result"
        line "}"

        blank
        line $ "var jsonData: Data? {"
        indent do
            line "let encoder = JSONEncoder()"
            line "return try? encoder.encode(self)"
        line "}"
            
        blank
        line $ "var jsonString: String? {"
        indent do
            line $ "guard let data = self.jsonData else { return nil }"
            line $ "return String(data: data, encoding: .utf8)"
        line "}"
    line "}"


renderClassExtension3 :: String -> Map String IRType -> Doc Unit
renderClassExtension3 className properties = do
    let forbidden = keywords <> ["jsonUntyped", "json"]
    let propertyNames = makePropertyNames properties "" forbidden
    line $ "extension " <> className <> " {"
    indent do
        line $ "fileprivate init?(fromAny any: Any) {"
        unless (M.isEmpty properties) $ indent do
            line "guard let json = any as? [String: Any] else { return nil }"
            let forbiddenForUntyped = forbidden <> (A.fromFoldable $ M.keys propertyNames)
            let untypedNames = makePropertyNames properties "Any" forbiddenForUntyped
            let forbiddenForConverted = forbiddenForUntyped <> (A.fromFoldable $ M.keys untypedNames)
            forEachProperty_ properties untypedNames \pname ptype untypedName _ -> do
                when (canBeNull ptype) do
                    line $ "let " <> untypedName <> " = removeNSNull(json[\"" <> stringEscape pname <> "\"])"
            line "guard"
            indent do
                forEachProperty_ properties untypedNames \pname ptype untypedName isLast -> do
                    let convertedName = lookupName pname propertyNames
                    unless (canBeNull ptype) do
                        line $ "let " <> untypedName <> " = removeNSNull(json[\"" <> stringEscape pname <> "\"]),"
                    convertCode <- convertAny ptype untypedName
                    line $ "let " <> convertedName <> " = " <> convertCode <> (if isLast then "" else ",")
                line "else { return nil }"
            forEachProperty_ properties propertyNames \pname _ fieldName _ -> do
                let convertedName = lookupName pname propertyNames
                line $ "self." <> fieldName <> " = " <> convertedName
        line "}"
        blank
        line "fileprivate var any: Any {"
        indent do
            if null properties
                then line "return [String: Any]()"
                else do
                    line "return ["
                    indent do
                        forEachProperty_ properties propertyNames \pname ptype fieldName _ -> do
                            convertCode <- convertToAny ptype ("self." <> fieldName)
                            line $ "\"" <> stringEscape pname <> "\": " <> convertCode <> ","
                    line "]"
        line "}"
    line "}"

renderClassExtension4 :: String -> Map String IRType -> Doc Unit
renderClassExtension4 className properties = do
    let propertyNames = makePropertyNames properties "" keywords
    when (M.size propertyNames > 0) do
        line $ "extension " <> className <> " {"
        indent do
            line "enum CodingKeys : String, CodingKey {"
            indent do
                for_ (M.toUnfoldable propertyNames :: Array (Tuple String String)) \(Tuple jsonName swiftName) -> do
                    line $ "case " <> swiftName <> " = \"" <> stringEscape jsonName <> "\""
            line "}"
        line "}"

makePropertyNames :: Map String IRType -> String -> Array String -> Map String String
makePropertyNames properties suffix forbidden =
    transformPropertyNames (fieldNamer suffix) otherField forbidden properties
    where
        fieldNamer :: String -> Namer String
        fieldNamer suffix' = simpleNamer \name -> swiftNameStyle false name <> suffix'

        otherField :: String -> String
        otherField name = "other" <> capitalize name

renderUnionDefinition :: Boolean -> String -> IRUnionRep -> Doc Unit
renderUnionDefinition codable unionName unionRep = do
    let { hasNull, nonNullUnion } = removeNullFromUnion unionRep
    line $ "enum " <> unionName <> codableString codable <> " {"
    indent do
        forUnion_ nonNullUnion \typ -> do
            name <- caseName typ
            rendered <- renderType typ
            line $ "case " <> name <> "(" <> rendered <> ")"
        when hasNull do
            name <- caseName IRNull
            line $ "case " <> name
    line "}"

renderUnionExtension3 :: String -> IRUnionRep -> Doc Unit
renderUnionExtension3 unionName unionRep = do
    let { hasNull, nonNullUnion } = removeNullFromUnion unionRep
    line $ "extension " <> unionName <> " {"
    indent do
        line $ "fileprivate static func fromJson(_ v: Any) -> " <> unionName <> "? {"
        indent do
            when hasNull do
                name <- caseName IRNull
                line "guard let v = removeNSNull(v)"
                line "else {"
                indent do
                    line $ "return ." <> name
                line "}"
            when (isUnionMember IRBool nonNullUnion) do
                renderCase IRBool
            when (isUnionMember IRInteger nonNullUnion) do
                renderCase IRInteger
            -- FIXME: this is ugly and inefficient
            for_ (L.difference (unionToList nonNullUnion) $ L.fromFoldable [IRBool, IRInteger]) \typ -> do
                renderCase typ
            line "return nil"
        line "}"
        blank
        line $ "fileprivate var any: Any {"
        indent do
            line $ "switch self {"
            forUnion_ unionRep \typ -> do
                name <- caseName typ
                let letString = if typ == IRNull then "" else "(let x)"
                convertCode <- convertToAny typ "x"
                line $ "case ." <> name <> letString <> ": return " <> convertCode
            line "}"
        line "}"
    line "}"
    where
    renderCase :: IRType -> Doc Unit
    renderCase t = do
        convertCode <- convertAny t "v"
        name <- caseName t
        line $ "if let x = " <> convertCode <> " { return ." <> name <> "(x) }"

renderUnionExtension4 :: String -> IRUnionRep -> Doc Unit
renderUnionExtension4 unionName unionRep = do
    let { hasNull, nonNullUnion } = removeNullFromUnion unionRep
    line $ "extension " <> unionName <> " {"
    indent do
        line "init(from decoder: Decoder) throws {"
        indent do
            line "let container = try decoder.singleValueContainer()"
            when (isUnionMember IRBool nonNullUnion) do
                renderCase IRBool
            when (isUnionMember IRInteger nonNullUnion) do
                renderCase IRInteger
            -- FIXME: this is ugly and inefficient
            for_ (L.difference (unionToList nonNullUnion) $ L.fromFoldable [IRBool, IRInteger]) \typ -> do
                renderCase typ
            when hasNull do
                name <- caseName IRNull
                line "if container.decodeNil() {"
                indent do
                    line $ "self = ." <> name
                    line "return"
                line "}"
            line $ "throw DecodingError.typeMismatch(" <> unionName <> ".self, DecodingError.Context(codingPath: decoder.codingPath, debugDescription: \"Wrong type for " <> unionName <> "\"))"
        line "}"
        blank
        line "func encode(to encoder: Encoder) throws {"
        indent do
            line "var container = encoder.singleValueContainer()"
            line "switch self {"
            for_ (unionToList nonNullUnion) \t -> do
                name <- caseName t
                line $ "case ." <> name <> "(let x):"
                indent do
                    line "try container.encode(x)"
            when hasNull do
                name <- caseName IRNull
                line $ "case ." <> name <> ":"
                indent do
                    line "try container.encodeNil()"
            line "}"
        line "}"
    line "}"
    where
        renderCase :: IRType -> Doc Unit
        renderCase t = do
            name <- caseName t
            typeName <- renderType t
            line $ "if let x = try? container.decode(" <> typeName <> ".self) {"
            indent do
                line $ "self = ." <> name <> "(x)"
                line "return"
            line "}"

caseName :: IRType -> Doc String
caseName t = swiftNameStyle false <$> getTypeNameForUnion t
