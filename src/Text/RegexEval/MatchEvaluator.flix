/*
 * Copyright 2020 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */



namespace Text/RegexEval {

    use Text/Regex.{Matcher};

    ///
    /// Matcher is a monad (handle, error).
    ///
    pub enum MatchEvaluator[r: Region, a: Type](Matcher[r] -> Result[a, String])


    instance Functor[MatchEvaluator[r]] {
        pub def map(f: a -> b & ef, o: MatchEvaluator[r, a]): MatchEvaluator[r, b] & ef =
            Text/RegexEval/MatchEvaluator.map(f, o)
    }

    instance Applicative[MatchEvaluator[r]] {
        pub def point(x: a): MatchEvaluator[r, a] = 
            Text/RegexEval/MatchEvaluator.point(x)

        pub def ap(f: MatchEvaluator[r, a -> b & ef], x: MatchEvaluator[r, a]): MatchEvaluator[r, b] & ef =
            Text/RegexEval/MatchEvaluator.ap(f, x)
    }

    instance Monad[MatchEvaluator[r]] {
        pub def flatMap(f: a -> MatchEvaluator[r, b] & ef, o: MatchEvaluator[r, a]): MatchEvaluator[r, b] & ef =
            Text/RegexEval/MatchEvaluator.flatMap(f, o)
    }

    instance MonadZero[MatchEvaluator[r]] {
        pub def empty(): MatchEvaluator[r, a] = Text/RegexEval/MatchEvaluator.empty()
    }

    instance MonadLib.MonadOr[MatchEvaluator[r]] {
        pub def orelse(a: MatchEvaluator[r, a], b: MatchEvaluator[r, a]): MatchEvaluator[r, a] =
            Text/RegexEval/MatchEvaluator.orelse(a, b)
    }

}

namespace Text/RegexEval/MatchEvaluator {

    use Applicative.{liftA2};

    use Text/RegexEval.MatchEvaluator;
    use Text/RegexEval.MatchEvaluator.{MatchEvaluator};

    use Text/Regex.Pattern;
    use Text/Regex.Pattern.{Pattern};
    use Text/Regex.Matcher;
    

    pub def point(x: a): MatchEvaluator[r, a] = MatchEvaluator(_ -> Ok(x))


    def apply1(x: MatchEvaluator[r, a], m: Matcher[r]): Result[a, String] =
        let MatchEvaluator(f) = x;
        f(m)

    pub def throwError(msg: String): MatchEvaluator[r, a] =
        MatchEvaluator(_ -> Err(msg))

    pub def swapError(msg: String, mx: MatchEvaluator[r, a]): MatchEvaluator[r, a] =
        MatchEvaluator(r -> match apply1(mx, r) {
            case Ok(a) => Ok(a)
            case Err(_) => Err(msg)
        })


    pub def map(f: a -> b & ef, x: MatchEvaluator[r, a]): MatchEvaluator[r, b] & ef =
        MatchEvaluator(s ->
            match apply1(x, s) {
                case Ok(a) => {
                    let ans = f(a) as & Pure;
                    Ok(ans)
                }
                case Err(e) => Err(e)
            }
        ) as & ef

    pub def ap(mf: MatchEvaluator[r, a -> b & ef], ma: MatchEvaluator[r, a]): MatchEvaluator[r, b] & ef =
        MatchEvaluator(s ->
            match apply1(mf, s) {
                case Ok(f) => match apply1(ma, s) {
                    case Ok(a) => Ok(f(a) as & Pure)
                    case Err(e2) => Err(e2)
                }
                case Err(e1) => Err(e1)
            }
        ) as & ef

    
    pub def flatMap(k: a -> MatchEvaluator[r, b] & ef, x: MatchEvaluator[r, a]): MatchEvaluator[r, b] & ef =
        MatchEvaluator(s ->
            match apply1(x, s) {
                case Ok(a) => apply1(k(a) as & Pure, s)
                case Err(e) => Err(e)
            }
        ) as & ef
    
    pub def bind(x: MatchEvaluator[r, a], k: a -> MatchEvaluator[r, b] & ef): MatchEvaluator[r, b] & ef =
        flatMap(k, x)

    // MatchEvaluator can be MonadOr because it does not "step".
    // It is only run on the current match.

    pub def empty(): MatchEvaluator[r, a] = throwError("empty")

    pub def orelse(ma: MatchEvaluator[r, a], mb: MatchEvaluator[r, a]): MatchEvaluator[r, a] =
        MatchEvaluator(s ->
            match apply1(ma, s) {
                case Ok(a) => Ok(a)
                case Err(_) => apply1(mb, s)
            }
        )


    pub def liftPrimResult(fn: Matcher[r] -> Result[a, String] & ef): MatchEvaluator[r, a] & ef =
        MatchEvaluator(r -> fn(r) as & Pure) as & ef


    pub def liftOption(err: String, x: Option[a]): MatchEvaluator[r, a] = match x {
        case Some(x1) => point(x1)
        case None     => throwError(err)
    }


    pub def liftResult(x: Result[a, String]): MatchEvaluator[r, a] =
        MatchEvaluator(_ -> x)

    pub def liftOperation(fn: Unit -> Result[a, String] & ef): MatchEvaluator[r, a] & ef =
        MatchEvaluator(_ -> fn() as & Pure) as & ef


    pub def optional(mx: MatchEvaluator[r, a]): MatchEvaluator[r, Option[a]] =
        MatchEvaluator(s -> match apply1(mx, s) {
            case Ok(a)  => Ok(Some(a))
            case Err(_) => Ok(None)
        })

    pub def withDefault(d: a, mx: MatchEvaluator[r, a]): MatchEvaluator[r, a] =
        MatchEvaluator(s -> match apply1(mx, s) {
            case Ok(a)  => Ok(a)
            case Err(_) => Ok(d)
        })



    pub def content(): MatchEvaluator[r, String] \ Read(r) =         
        liftPrimResult(Text/Regex/Matcher.content)


    /// TODO - is this worthwhile?
    pub def transformContent(f: String -> a & ef): MatchEvaluator[r, a] \ {ef, Read(r)} = map(f, content())


    pub def getStart(): MatchEvaluator[r, Int32] \ Read(r) = 
        liftPrimResult(Text/Regex/Matcher.start)

    pub def getEnd(): MatchEvaluator[r, Int32] \ Read(r) =
        liftPrimResult(Text/Regex/Matcher.end)

    pub def getLength(): MatchEvaluator[r, Int32] \ Read(r) =
        liftA2((x,y) -> y - x, getStart(), getEnd()) |> swapError("getLength")

    ///
    ///
    pub def getBounds(): MatchEvaluator[r, {start :: Int32, end :: Int32}] \ Read(r) =
        liftA2((x,y) -> {start = x, end = y}, getStart(), getEnd()) |> swapError("getBounds")


    ///
    /// Note - this function uses 0-indexing, the equivalent Java method `group(int group)` uses 1-indexing.
    ///
    pub def getCaptureAt(ix: Int32): MatchEvaluator[r, String] \ Read(r) =
        liftPrimResult(Text/Regex/Matcher.groupAt(ix))

    pub def getCaptureNamed(label: String): MatchEvaluator[r, String] \ Read(r) =
        liftPrimResult(Text/Regex/Matcher.groupNamed(label))

/*
    ///
    /// Note - this function uses 0-indexing, the equivalent Java method `group(int group)` uses 1-indexing.
    ///
    pub def getCaptureStartAt(ix: Int32): MatchEvaluator[Int32] =
        liftPrimResult(x -> Text/Regex/Internal/Prim.startWithIndex(x, ix+1))
            |> swapError("getCaptureStartAt(${ix})")

    pub def getCaptureStartNamed(label: String): MatchEvaluator[Int32] =
        liftPrimResult(x -> Text/Regex/Internal/Prim.startWithLabel(x, label))
            |> swapError("getCaptureStartNamed('${label}')")

    ///
    /// Note - this function uses 0-indexing, the equivalent Java method `group(int group)`
    /// uses 1-indexing.
    ///
    pub def getCaptureEndAt(ix: Int32): MatchEvaluator[Int32] =
        liftPrimResult(x -> Text/Regex/Internal/Prim.endWithIndex(x, ix+1))
            |> swapError("getCaptureEndAt(${ix})")

    pub def getCaptureEndNamed(label: String): MatchEvaluator[Int32] =
        liftPrimResult(x -> Text/Regex/Internal/Prim.endWithLabel(x, label))
            |> swapError("getCaptureEndNamed('${label}')")

    ///
    /// Note - this function uses 0-indexing, the equivalent Java method `group(int group)`
    /// uses 1-indexing.
    ///
    pub def getCaptureRangeAt(ix: Int32): MatchEvaluator[{start :: Int32, end :: Int32}] =
        liftA2((x,y) -> {start = x, end = y}, getCaptureStartAt(ix), getCaptureEndAt(ix))
            |> swapError("getCaptureRangeAt(${ix})")

    pub def getCaptureRangeNamed(label: String): MatchEvaluator[{start :: Int32, end :: Int32}] =
        liftA2((x, y) -> {start = x, end = y}, getCaptureStartNamed(label), getCaptureEndNamed(label))
            |> swapError("getCaptureRangeNamed('${label}')")


*/
    pub def getInt8At(ix: Int32): MatchEvaluator[r, Int8] \ Read(r) =
        let* str = getCaptureAt(ix);
        liftOption("getInt8At(${ix})", FromString.fromString(str): Option[Int8])

    pub def getInt8Named(label: String): MatchEvaluator[r, Int8] \ Read(r) =
        let* str = getCaptureNamed(label);
        liftOption("getInt8Named('${label}')", FromString.fromString(str): Option[Int8])

    pub def getInt16At(ix: Int32): MatchEvaluator[r, Int16] \ Read(r) =
        let* str = getCaptureAt(ix);
        liftOption("getInt16At(${ix})", FromString.fromString(str): Option[Int16])

    pub def getInt16Named(label: String): MatchEvaluator[r, Int16] \ Read(r) =
        let* str = getCaptureNamed(label);
        liftOption("getInt16Named('${label}')", FromString.fromString(str): Option[Int16])

    pub def getInt32At(ix: Int32): MatchEvaluator[r, Int32] \ Read(r) =
        let* str = getCaptureAt(ix);
        liftOption("getInt32At(${ix})", FromString.fromString(str): Option[Int32])

    pub def getInt32Named(label: String): MatchEvaluator[r, Int32] \ Read(r) =
        let* str = getCaptureNamed(label);
        liftOption("getInt32Named('${label}')", FromString.fromString(str): Option[Int32])

    pub def getInt64At(ix: Int32): MatchEvaluator[r, Int64] \ Read(r) =
        let* str = getCaptureAt(ix);
        liftOption("getInt64At(${ix})", FromString.fromString(str): Option[Int64])

    pub def getInt64Named(label: String): MatchEvaluator[r, Int64] \ Read(r) =
        let* str = getCaptureNamed(label);
        liftOption("getInt64Named('${label}')", FromString.fromString(str): Option[Int64])

    pub def getBigIntAt(ix: Int32): MatchEvaluator[r, BigInt] \ Read(r) =
        let* str = getCaptureAt(ix);
        liftOption("getBigIntAt(${ix})", FromString.fromString(str): Option[BigInt])

    pub def getBigIntNamed(label: String): MatchEvaluator[r, BigInt] \ Read(r) =
        let* str = getCaptureNamed(label);
        liftOption("getBigIntNamed('${label}')", FromString.fromString(str): Option[BigInt])

    pub def getFloat32At(ix: Int32): MatchEvaluator[r, Float32] \ Read(r) =
        let* str = getCaptureAt(ix);
        liftOption("getFloat32At(${ix})", FromString.fromString(str): Option[Float32])

    pub def getFloat32Named(label: String): MatchEvaluator[r, Float32] \ Read(r) =
        let* str = getCaptureNamed(label);
        liftOption("getFloat32Named('${label}')", FromString.fromString(str): Option[Float32])

    pub def getFloat64At(ix: Int32): MatchEvaluator[r, Float64] \ Read(r) =
        let* str = getCaptureAt(ix);
        liftOption("getFloat64At(${ix})", FromString.fromString(str): Option[Float64])

    pub def getFloat64Named(label: String): MatchEvaluator[r, Float64] \ Read(r) =
        let* str = getCaptureNamed(label);
        liftOption("getFloat64Named('${label}')", FromString.fromString(str): Option[Float64])

}
