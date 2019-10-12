namespace MXNetSharp.IO
open MXNetSharp.Interop
open System.Collections.Generic
open System
open MXNetSharp


type DataType = 
    | Float16
    | Float32
    | Float64
    | Int32
    | Int64
    | Int8
    | UInt8
    override x.ToString() =
        match x with
            | Float16 -> "float16"
            | Float32 -> "float32"
            | Float64 -> "float64"
            | Int32 -> "int32"
            | Int64 -> "int64"
            | Int8 -> "int8"
            | UInt8 -> "uint8"
    

exception DataIterNotFound of string with
    override x.Message = 
        match x :> Exception with 
        | DataIterNotFound(name) -> sprintf "Data iterator %s could not be found." name
        | _ -> failwith "unreachable"

type DataIterDefinition internal (handle, info, parameters : IDictionary<string, obj>) =
    static let lookup =
        lazy 
            MXDataIter.list()
            |> Seq.map (fun x -> DataIterDefinition(x, MXDataIter.getInfo x))
            |> Seq.map (fun x -> x.Name, x)
            |> dict 
            |> Dictionary
    new(handle, info) =  DataIterDefinition(handle, info, dict[])
    member x.DataIterCreatorHandle = handle
    member x.Name = info.Name
    member x.Info = info
    static member FromName(name) = 
        let scc,v = lookup.Value.TryGetValue(name)
        if scc then 
            v
        else    
            raise (DataIterNotFound name)
    member x.WithParameters(kvps : IDictionary<string,obj>) = 
        let d = Dictionary(parameters)
        kvps |> Seq.iter (fun kvp -> d.[kvp.Key] <- kvp.Value)
        DataIterDefinition(handle, info, d)
    member x.GetParameters() = Dictionary(parameters) :> IDictionary<_,_>

//TODO: should be disposable, freeing iterHandle
type DataIter(definition : DataIterDefinition) = 
    let mutable atEnd = false
    let parameters = definition.GetParameters()
    let iterHandle = 
        let keys,vals = 
            parameters
            |> Seq.filter (fun x -> x.Value |> isNull |> not)
            |> Seq.map
                (fun kvp ->
                    kvp.Key, 
                        match kvp.Value with 
                        | :? bool as x -> if x then "1" else "0"
                        | :? seq<int> as x -> x |> Seq.map string |> String.concat ", " |> sprintf "[%s]"
                        | x -> x.ToString()
                )
            |> Seq.toArray
            |> Array.unzip
        MXDataIter.create definition.DataIterCreatorHandle keys vals
    internal new(handle, info, parameters : IDictionary<string, obj>) = 
        DataIter(DataIterDefinition(handle, info, parameters))
    member x.DataIterDefinition = definition
    member x.Reset() = 
        MXDataIter.beforeFirst iterHandle
        atEnd <- false
    member x.GetData() = MXDataIter.getData iterHandle |> NDArray
    member x.GetIndex() = MXDataIter.getIndex iterHandle
    member x.GetPadNum() = MXDataIter.getPadNum iterHandle
    member x.GetLabel() = MXDataIter.getLabel iterHandle |> NDArray
    member x.Next() = 
        atEnd <- atEnd || not (MXDataIter.next iterHandle > 0)
        not atEnd


/// <summary>Returns the CSV file iterator.
/// 
/// In this function, the `data_shape` parameter is used to set the shape of each line of the input data.
/// If a row in an input file is `1,2,3,4,5,6`` and `data_shape` is (3,2), that row
/// will be reshaped, yielding the array [[1,2],[3,4],[5,6]] of shape (3,2).
/// 
/// By default, the `CSVIter` has `round_batch` parameter set to ``True``. So, if `batch_size`
/// is 3 and there are 4 total rows in CSV file, 2 more examples
/// are consumed at the first round. If `reset` function is called after first round,
/// the call is ignored and remaining examples are returned in the second round.
/// 
/// If one wants all the instances in the second round after calling `reset`, make sure
/// to set `round_batch` to False.
/// 
/// If ``data_csv = &#39;data/&#39;`` is set, then all the files in this directory will be read.
/// 
/// ``reset()`` is expected to be called only after a complete pass of data.
/// 
/// By default, the CSVIter parses all entries in the data file as float32 data type,
/// if `dtype` argument is set to be &#39;int32&#39; or &#39;int64&#39; then CSVIter will parse all entries in the file
/// as int32 or int64 data type accordingly.
/// 
/// Examples::
/// 
///   // Contents of CSV file ``data/data.csv``.
///   1,2,3
///   2,3,4
///   3,4,5
///   4,5,6
/// 
///   // Creates a `CSVIter` with `batch_size`=2 and default `round_batch`=True.
///   CSVIter = mx.io.CSVIter(data_csv = &#39;data/data.csv&#39;, data_shape = (3,),
///   batch_size = 2)
/// 
///   // Two batches read from the above iterator are as follows:
///   [[ 1.  2.  3.]
///   [ 2.  3.  4.]]
///   [[ 3.  4.  5.]
///   [ 4.  5.  6.]]
/// 
///   // Creates a `CSVIter` with default `round_batch` set to True.
///   CSVIter = mx.io.CSVIter(data_csv = &#39;data/data.csv&#39;, data_shape = (3,),
///   batch_size = 3)
/// 
///   // Two batches read from the above iterator in the first pass are as follows:
///   [[1.  2.  3.]
///   [2.  3.  4.]
///   [3.  4.  5.]]
/// 
///   [[4.  5.  6.]
///   [1.  2.  3.]
///   [2.  3.  4.]]
/// 
///   // Now, `reset` method is called.
///   CSVIter.reset()
/// 
///   // Batch read from the above iterator in the second pass is as follows:
///   [[ 3.  4.  5.]
///   [ 4.  5.  6.]
///   [ 1.  2.  3.]]
/// 
///   // Creates a `CSVIter` with `round_batch`=False.
///   CSVIter = mx.io.CSVIter(data_csv = &#39;data/data.csv&#39;, data_shape = (3,),
///   batch_size = 3, round_batch=False)
/// 
///   // Contents of two batches read from the above iterator in both passes, after calling
///   // `reset` method before second pass, is as follows:
///   [[1.  2.  3.]
///   [2.  3.  4.]
///   [3.  4.  5.]]
/// 
///   [[4.  5.  6.]
///   [2.  3.  4.]
///   [3.  4.  5.]]
/// 
///   // Creates a &#39;CSVIter&#39; with `dtype`=&#39;int32&#39;
///   CSVIter = mx.io.CSVIter(data_csv = &#39;data/data.csv&#39;, data_shape = (3,),
///   batch_size = 3, round_batch=False, dtype=&#39;int32&#39;)
/// 
///   // Contents of two batches read from the above iterator in both passes, after calling
///   // `reset` method before second pass, is as follows:
///   [[1  2  3]
///   [2  3  4]
///   [3  4  5]]
/// 
///   [[4  5  6]
///   [2  3  4]
///   [3  4  5]]
/// 
/// 
/// 
/// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\io\iter_csv.cc:L308</summary>
type CSVIter private (creatorHandle : IntPtr, info : DataIterInfo,
             dataCsv : string, 
             dataShape : int [], 
             labelCsv : string, 
             labelShape : int [], 
             batchSize : int, 
             roundBatch : bool, 
             prefetchBuffer : int64, 
             ctx : string,
             dtype : DataType option) =
    inherit DataIter(creatorHandle,info, 
        dict["data_csv", box dataCsv; 
             "data_shape", box dataShape
             "label_csv", box labelCsv
             "label_shape", box labelShape
             "batch_size", box batchSize
             "round_batch", box roundBatch
             "prefetch_buffer", box prefetchBuffer
             "ctx", box ctx
             "dtype", box dtype])
    do 
        if batchSize < 0 then 
            invalidArg "batchSize" (sprintf "CSVIter (%s): batch size must be non-negative" dataCsv)
        
    /// <summary>Returns the CSV file iterator.
    /// 
    /// In this function, the `data_shape` parameter is used to set the shape of each line of the input data.
    /// If a row in an input file is `1,2,3,4,5,6`` and `data_shape` is (3,2), that row
    /// will be reshaped, yielding the array [[1,2],[3,4],[5,6]] of shape (3,2).
    /// 
    /// By default, the `CSVIter` has `round_batch` parameter set to ``True``. So, if `batch_size`
    /// is 3 and there are 4 total rows in CSV file, 2 more examples
    /// are consumed at the first round. If `reset` function is called after first round,
    /// the call is ignored and remaining examples are returned in the second round.
    /// 
    /// If one wants all the instances in the second round after calling `reset`, make sure
    /// to set `round_batch` to False.
    /// 
    /// If ``data_csv = &#39;data/&#39;`` is set, then all the files in this directory will be read.
    /// 
    /// ``reset()`` is expected to be called only after a complete pass of data.
    /// 
    /// By default, the CSVIter parses all entries in the data file as float32 data type,
    /// if `dtype` argument is set to be &#39;int32&#39; or &#39;int64&#39; then CSVIter will parse all entries in the file
    /// as int32 or int64 data type accordingly.
    /// 
    /// Examples::
    /// 
    ///   // Contents of CSV file ``data/data.csv``.
    ///   1,2,3
    ///   2,3,4
    ///   3,4,5
    ///   4,5,6
    /// 
    ///   // Creates a `CSVIter` with `batch_size`=2 and default `round_batch`=True.
    ///   CSVIter = mx.io.CSVIter(data_csv = &#39;data/data.csv&#39;, data_shape = (3,),
    ///   batch_size = 2)
    /// 
    ///   // Two batches read from the above iterator are as follows:
    ///   [[ 1.  2.  3.]
    ///   [ 2.  3.  4.]]
    ///   [[ 3.  4.  5.]
    ///   [ 4.  5.  6.]]
    /// 
    ///   // Creates a `CSVIter` with default `round_batch` set to True.
    ///   CSVIter = mx.io.CSVIter(data_csv = &#39;data/data.csv&#39;, data_shape = (3,),
    ///   batch_size = 3)
    /// 
    ///   // Two batches read from the above iterator in the first pass are as follows:
    ///   [[1.  2.  3.]
    ///   [2.  3.  4.]
    ///   [3.  4.  5.]]
    /// 
    ///   [[4.  5.  6.]
    ///   [1.  2.  3.]
    ///   [2.  3.  4.]]
    /// 
    ///   // Now, `reset` method is called.
    ///   CSVIter.reset()
    /// 
    ///   // Batch read from the above iterator in the second pass is as follows:
    ///   [[ 3.  4.  5.]
    ///   [ 4.  5.  6.]
    ///   [ 1.  2.  3.]]
    /// 
    ///   // Creates a `CSVIter` with `round_batch`=False.
    ///   CSVIter = mx.io.CSVIter(data_csv = &#39;data/data.csv&#39;, data_shape = (3,),
    ///   batch_size = 3, round_batch=False)
    /// 
    ///   // Contents of two batches read from the above iterator in both passes, after calling
    ///   // `reset` method before second pass, is as follows:
    ///   [[1.  2.  3.]
    ///   [2.  3.  4.]
    ///   [3.  4.  5.]]
    /// 
    ///   [[4.  5.  6.]
    ///   [2.  3.  4.]
    ///   [3.  4.  5.]]
    /// 
    ///   // Creates a &#39;CSVIter&#39; with `dtype`=&#39;int32&#39;
    ///   CSVIter = mx.io.CSVIter(data_csv = &#39;data/data.csv&#39;, data_shape = (3,),
    ///   batch_size = 3, round_batch=False, dtype=&#39;int32&#39;)
    /// 
    ///   // Contents of two batches read from the above iterator in both passes, after calling
    ///   // `reset` method before second pass, is as follows:
    ///   [[1  2  3]
    ///   [2  3  4]
    ///   [3  4  5]]
    /// 
    ///   [[4  5  6]
    ///   [2  3  4]
    ///   [3  4  5]]
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\io\iter_csv.cc:L308</summary>
    /// <param name="dataCsv">The input CSV file or a directory path.</param>
    /// <param name="dataShape">The shape of one example.</param>
    /// <param name="labelCsv">The input CSV file or a directory path. If NULL, all labels will be returned as 0.</param>
    /// <param name="labelShape">The shape of one label.</param>
    /// <param name="batchSize">Batch size.</param>
    /// <param name="roundBatch">Whether to use round robin to handle overflow batch or not.</param>
    /// <param name="prefetchBuffer">Maximum number of batches to prefetch.</param>
    /// <param name="ctx">Context data loader optimized for.</param>
    /// <param name="dtype">Output data type. ``None`` means no change.</param>
    new(dataCsv : string, 
        dataShape : int seq, 
        ?labelCsv : string, 
        ?labelShape : int seq, 
        ?batchSize : int, 
        ?roundBatch : bool, 
        ?prefetchBuffer : int64, 
        ?deviceType : DeviceType, 
        ?dtype : DataType) =
        let def = DataIterDefinition.FromName("CSVIter")
        let dataShape = dataShape |> Seq.toArray
        let labelCsv = defaultArg labelCsv null
        let labelShape = defaultArg labelShape (Seq.singleton 1) |> Seq.toArray
        let batchSize = defaultArg batchSize 0 //REVIEW: maybe default to 1?
        let roundBatch = defaultArg roundBatch true
        let prefetchBuffer = defaultArg prefetchBuffer 4L
        let deviceType = 
            match defaultArg deviceType DeviceType.GPU with 
            | DeviceType.CPU | DeviceType.CPUPinned -> "cpu"
            | _ -> "gpu"
        CSVIter(def.DataIterCreatorHandle, def.Info, dataCsv, dataShape, labelCsv, labelShape, batchSize, roundBatch, prefetchBuffer, deviceType, dtype)
    /// The input CSV file or a directory path.
    member x.DataCsv = dataCsv
    /// The shape of one example.
    member x.DataShape = dataShape |> Array.toSeq
    /// The input CSV file or a directory path. If None, all labels will be returned as 0.
    member x.LabelCsv = if String.IsNullOrEmpty labelCsv then None else Some labelCsv
    /// The shape of one label.
    member x.LabelShape = labelShape |> Array.toSeq
    /// Batch size.
    member x.BatchSize = batchSize
    /// Whether to use round robin to handle overflow batch or not.
    member x.RoundBatch = roundBatch
    /// Maximum number of batches to prefetch.
    member x.PrefetchBuffer = prefetchBuffer
    /// Context data loader optimized for.
    member x.DeviceType = 
        match ctx.ToLower() with 
        | "cpu" -> DeviceType.CPU
        | "gpu" -> DeviceType.GPU
        | _ -> failwith "Internal error in CSVIter. Device type can only be one of 'cpu' or 'gpu'"
    /// Output data type. ``None`` means no change.
    member x.DataType = dtype




/// <summary>Iterating on the MNIST dataset.
/// 
/// One can download the dataset from http://yann.lecun.com/exdb/mnist/
/// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\io\iter_mnist.cc:L265</summary>
/// <param name="image">Dataset Param: Mnist image path.</param>
/// <param name="label">Dataset Param: Mnist label path.</param>
/// <param name="batchSize">Batch Param: Batch Size.</param>
/// <param name="shuffle">Augmentation Param: Whether to shuffle data.</param>
/// <param name="flat">Augmentation Param: Whether to flat the data into 1D.</param>
/// <param name="seed">Augmentation Param: Random Seed.</param>
/// <param name="silent">Auxiliary Param: Whether to print out data info.</param>
/// <param name="numParts">partition the data into multiple parts</param>
/// <param name="partIndex">the index of the part will read</param>
/// <param name="prefetchBuffer">Maximum number of batches to prefetch.</param>
/// <param name="ctx">Context data loader optimized for.</param>
/// <param name="dtype">Output data type. ``None`` means no change.</param>
type MNISTIter private (creatorHandle : IntPtr, info : DataIterInfo,
               image : string, 
               label : string, 
               batchSize : int, 
               shuffle : bool, 
               flat : bool, 
               seed : int, 
               silent : bool, 
               numParts : int, 
               partIndex : int, 
               prefetchBuffer : int64, 
               ctx : string, 
               dtype : DataType option) =
    inherit DataIter(creatorHandle,info, 
        dict["image", box image; 
             "label", box label
             "batch_size", box batchSize
             "shuffle", box shuffle
             "flat", box flat
             "seed", box seed
             "silent", box silent
             "num_parts", box numParts
             "part_index", box partIndex
             "prefetch_buffer", box prefetchBuffer
             "ctx", box ctx
             "dtype", box dtype])
    do 
        if batchSize < 0 then 
            invalidArg "batchSize" "MNISTIter: batch size must be non-negative"
    /// <summary>Iterating on the MNIST dataset.
    /// 
    /// One can download the dataset from http://yann.lecun.com/exdb/mnist/
    /// 
    /// 
    /// 
    /// Defined in C:\Jenkins\workspace\mxnet-tag\mxnet\src\io\iter_mnist.cc:L265</summary>
    /// <param name="image">Dataset Param: Mnist image path.</param>
    /// <param name="label">Dataset Param: Mnist label path.</param>
    /// <param name="batchSize">Batch Param: Batch Size.</param>
    /// <param name="shuffle">Augmentation Param: Whether to shuffle data.</param>
    /// <param name="flat">Augmentation Param: Whether to flat the data into 1D.</param>
    /// <param name="seed">Augmentation Param: Random Seed.</param>
    /// <param name="silent">Auxiliary Param: Whether to print out data info.</param>
    /// <param name="numParts">partition the data into multiple parts</param>
    /// <param name="partIndex">the index of the part will read</param>
    /// <param name="prefetchBuffer">Maximum number of batches to prefetch.</param>
    /// <param name="ctx">Context data loader optimized for.</param>
    /// <param name="dtype">Output data type. ``None`` means no change.</param>
    new (?image : string, 
         ?label : string, 
         ?batchSize : int, 
         ?shuffle : bool, 
         ?flat : bool, 
         ?seed : int, 
         ?silent : bool, 
         ?numParts : int, 
         ?partIndex : int, 
         ?prefetchBuffer : int64, 
         ?deviceType : DeviceType, 
         ?dataType : DataType) =
        let def = DataIterDefinition.FromName("MNISTIter")
        let image = defaultArg image "./train-images-idx3-ubyte"
        let label = defaultArg label "./train-labels-idx1-ubyte"
        let batchSize = defaultArg batchSize 128
        let shuffle = defaultArg shuffle true
        let flat = defaultArg flat false
        let seed = defaultArg seed 0
        let silent = defaultArg silent false
        let numParts = defaultArg numParts 1
        let partIndex = defaultArg partIndex 0
        let prefetchBuffer = defaultArg prefetchBuffer 4L
        let deviceType = 
            match defaultArg deviceType DeviceType.GPU with 
            | DeviceType.CPU | DeviceType.CPUPinned -> "cpu"
            | _ -> "gpu"
        MNISTIter(def.DataIterCreatorHandle,def.Info,
                  image,
                  label,
                  batchSize,
                  shuffle,
                  flat,
                  seed,
                  silent,
                  numParts,
                  partIndex,
                  prefetchBuffer,
                  deviceType,
                  dataType)
    /// Dataset Param: Mnist image path.
    member __.Image = image
    /// Dataset Param: Mnist label path.
    member __.Label = label
    /// Batch Param: Batch Size.
    member __.BatchSize = batchSize 
    /// Augmentation Param: Whether to shuffle data.
    member __.Shuffle = shuffle
    /// Augmentation Param: Whether to flat the data into 1D.
    member __.Flat = flat
    /// Augmentation Param: Random Seed.
    member __.Seed = seed
    /// Auxiliary Param: Whether to print out data info.
    member __.Silent = silent
    /// partition the data into multiple parts
    member __.NumParts = numParts
    /// the index of the part will read
    member __.PartIndex = partIndex
    /// Maximum number of batches to prefetch.
    member __.PrefetchBuffer = prefetchBuffer
    /// Context data loader optimized for.
    member x.DeviceType = 
        match ctx.ToLower() with 
        | "cpu" -> DeviceType.CPU
        | "gpu" -> DeviceType.GPU
        | _ -> failwith "Internal error in CSVIter. Device type can only be one of 'cpu' or 'gpu'"
    /// Output data type. ``None`` means no change.
    member x.DataType = dtype


        
