package main.scala

import org.apache.spark.mllib.feature._
import org.apache.spark.mllib.linalg.Vectors
import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.mllib.tree.{DecisionTree, RandomForest, PCARD}
import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.rdd.RDD
import java.io.{File, PrintWriter}

object runSuite{

// ROS

def ROS(train: RDD[LabeledPoint], overRate: Double): RDD[LabeledPoint] = {
  var oversample: RDD[LabeledPoint] = train.sparkContext.emptyRDD

  val train_positive = train.filter(_.label == 1)
  val train_negative = train.filter(_.label == 0)
  val num_neg = train_negative.count().toDouble
  val num_pos = train_positive.count().toDouble

  if (num_pos > num_neg) {
    val fraction = (num_pos * overRate) / num_neg
    oversample = train_positive.union(train_negative.sample(withReplacement = true, fraction))
  } else {
    val fraction = (num_neg * overRate) / num_pos
    oversample = train_negative.union(train_positive.sample(withReplacement = true, fraction))
  }
  oversample.repartition(train.getNumPartitions)
}


// RUS

def RUS(train: RDD[LabeledPoint]): RDD[LabeledPoint] = {
  var undersample: RDD[LabeledPoint] = train.sparkContext.emptyRDD

  val train_positive = train.filter(_.label == 1)
  val train_negative = train.filter(_.label == 0)
  val num_neg = train_negative.count().toDouble
  val num_pos = train_positive.count().toDouble

  if (num_pos > num_neg) {
    val fraction = num_neg / num_pos
    undersample = train_negative.union(train_positive.sample(withReplacement = false, fraction))
  } else {
    val fraction = num_pos / num_neg
    undersample = train_positive.union(train_negative.sample(withReplacement = false, fraction))
  }
  undersample
}

def tprtnr(labelAndPreds: RDD[(Double, Double)]): Double = {
val tp=labelAndPreds.filter(r=> r._1 == r._2 && r._1==1.0).count().toDouble
val tn=labelAndPreds.filter(r=> r._1 == r._2 && r._1==0.0).count().toDouble
val fp=labelAndPreds.filter(r=> r._1 != r._2 && r._1==0.0).count().toDouble
val fn=labelAndPreds.filter(r=> r._1 != r._2 && r._1==1.0).count().toDouble
(tp/(tp+fn))*(tn/(tn+fp))
}

  def main(arg: Array[String]) {
    //Basic setup
    val jobName = "BigData II Eilder"

    //Spark Configuration
    val conf = new SparkConf().setAppName(jobName)
    val sc = new SparkContext(conf)

    //Log level
    sc.setLogLevel("ERROR")

    // File locations
    // local
/*
    val headerPath = "file:///home/spark/datasets/susy.header"
    val trainPath = "file:///home/spark/datasets/susy-10k-tra.data"
    val testPath = "file:///home/spark/datasets/susy-10k-tst.data"
    val writerPath = "/home/spark/workspace/BigDataIIEilder/results.txt"
*/
   // cluster
    val headerPath = "/user/datasets/master/higgs/higgs.header"
    val trainPath = "/user/datasets/master/higgs/higgsMaster-Train.data"
    val testPath = "/user/datasets/master/higgs/higgsMaster-Test.data"
    val writerPath = "/home/xJ858376/results.txt"

    //Prepare result writer
    val writer = new PrintWriter(writerPath)

    //Load train and test with KeelParser
    val numPartition = 10
    val converter = new KeelParser(sc, headerPath)
    var train = sc.textFile(trainPath, numPartition)
    .map(line => converter.parserToLabeledPoint(line)).persist
    var test = sc.textFile(testPath, numPartition)
    .map(line => converter.parserToLabeledPoint(line)).persist
    
    //ROS and RUS dataset preparations
    var trainROS = ROS(train, 1.0).persist()
    val trainRUS = RUS(train).persist()
    
    //Third preprocessing algorithm filtering, FCNN
    val fcnn_mr_model = new FCNN_MR(train, 3)
    val trainFCNN = fcnn_mr_model.runPR()
    trainFCNN.persist()


    //HME-BD Noise Filter to the ROS dataset
    val nTrees = 100
    val maxDepthRF = 10
    val partitions = 4

    var hme_bd_model_noisy = new HME_BD(trainROS, nTrees, partitions, maxDepthRF, 48151623)
    var hme_bd_train = hme_bd_model_noisy.runFilter()
    hme_bd_train.persist()

    //Decision tree
    //  Empty categoricalFeaturesInfo indicates all features are continuous.
    val DTnumClasses = 2
    val DTcategoricalFeaturesInfo = Map[Int, Int]()
    val DTimpurity = "gini"
    val DTmaxDepth = 5
    val DTmaxBins = 32

    var modelDT = DecisionTree.trainClassifier(train, 
    DTnumClasses, DTcategoricalFeaturesInfo, DTimpurity, DTmaxDepth, DTmaxBins)

    // Evaluate model on test instances and compute test error
    var labelAndPreds = train.map { 
       point =>val prediction = modelDT.predict(point.features)
      (point.label, prediction)
    }

    writer.write("DT train TPR x TNR " + tprtnr(labelAndPreds) + "\n")

    labelAndPreds = test.map { point =>
      val prediction = modelDT.predict(point.features)
      (point.label, prediction)
    }
    writer.write("DT test TPR x TNR " + tprtnr(labelAndPreds) + "\n")

    modelDT = DecisionTree.trainClassifier(trainROS, 
    DTnumClasses, DTcategoricalFeaturesInfo, DTimpurity, DTmaxDepth, DTmaxBins)
    
    labelAndPreds = trainROS.map { point =>
      val prediction = modelDT.predict(point.features)
      (point.label, prediction)
    }
    writer.write("DT ROS train TPR x TNR " + tprtnr(labelAndPreds) + "\n")


    labelAndPreds = test.map { point =>
      val prediction = modelDT.predict(point.features)
      (point.label, prediction)
    }
    writer.write("DT ROS test TPR x TNR " + tprtnr(labelAndPreds) + "\n")


    modelDT = DecisionTree.trainClassifier(trainRUS, DTnumClasses, DTcategoricalFeaturesInfo, DTimpurity, DTmaxDepth, DTmaxBins)
    
    labelAndPreds = trainRUS.map { point =>
      val prediction = modelDT.predict(point.features)
      (point.label, prediction)
    }
    writer.write("DT RUS train TPR x TNR " + tprtnr(labelAndPreds) + "\n")


    labelAndPreds = test.map { point =>
      val prediction = modelDT.predict(point.features)
      (point.label, prediction)
    }
    writer.write("DT RUS test TPR x TNR " + tprtnr(labelAndPreds) + "\n")

    modelDT = DecisionTree.trainClassifier(trainFCNN, DTnumClasses, DTcategoricalFeaturesInfo, DTimpurity, DTmaxDepth, DTmaxBins)
    
    labelAndPreds = trainFCNN.map { point =>
      val prediction = modelDT.predict(point.features)
      (point.label, prediction)
    }
    writer.write("DT FCNN filter train TPR x TNR " + tprtnr(labelAndPreds) + "\n")


    labelAndPreds = test.map { point =>
      val prediction = modelDT.predict(point.features)
      (point.label, prediction)
    }
    writer.write("DT FCNN filter test TPR x TNR " + tprtnr(labelAndPreds) + "\n")

// Train a RandomForest model.
    // Empty categoricalFeaturesInfo indicates all features are continuous.
    val RFnumClasses = 2
    val RFcategoricalFeaturesInfo = Map[Int, Int]()
    var RFnumTrees = 100
    val RFfeatureSubsetStrategy = "auto" // Let the algorithm choose.
    val RFimpurity = "gini"
    val RFmaxDepth = 4
    val RFmaxBins = 32

    var modelRF = RandomForest.trainClassifier(train, RFnumClasses, 
    RFcategoricalFeaturesInfo, RFnumTrees, RFfeatureSubsetStrategy, 
    RFimpurity, RFmaxDepth, RFmaxBins)

    // Evaluate model on test instances and compute test error
    labelAndPreds = train.map { point =>
      val prediction = modelRF.predict(point.features)
      (point.label, prediction)
    }
    writer.write("RF train TPR x TNR " + tprtnr(labelAndPreds) + "\n")


    labelAndPreds = test.map { point =>
      val prediction = modelRF.predict(point.features)
      (point.label, prediction)
    }
    writer.write("RF test TPR x TNR " + tprtnr(labelAndPreds) + "\n")


    modelRF = RandomForest.trainClassifier(trainROS, RFnumClasses, 
    RFcategoricalFeaturesInfo, RFnumTrees, RFfeatureSubsetStrategy, 
    RFimpurity, RFmaxDepth, RFmaxBins)

    // Evaluate model on test instances and compute test error
    labelAndPreds = trainROS.map { point =>
      val prediction = modelRF.predict(point.features)
      (point.label, prediction)
    }
    writer.write("RF ROS train TPR x TNR " + tprtnr(labelAndPreds) + "\n")


    labelAndPreds = test.map { point =>
      val prediction = modelRF.predict(point.features)
      (point.label, prediction)
    }
    writer.write("RF ROS test TPR x TNR " + tprtnr(labelAndPreds) + "\n")


    modelRF = RandomForest.trainClassifier(trainRUS, RFnumClasses, RFcategoricalFeaturesInfo, RFnumTrees, RFfeatureSubsetStrategy, RFimpurity, RFmaxDepth, RFmaxBins)

    // Evaluate model on test instances and compute test error
    labelAndPreds = trainRUS.map { point =>
      val prediction = modelRF.predict(point.features)
      (point.label, prediction)
    }
    writer.write("RF RUS train TPR x TNR " + tprtnr(labelAndPreds) + "\n")

    labelAndPreds = test.map { point =>
      val prediction = modelRF.predict(point.features)
      (point.label, prediction)
    }
    writer.write("RF RUS test TPR x TNR " + tprtnr(labelAndPreds) + "\n")
    
    modelRF = RandomForest.trainClassifier(trainFCNN, RFnumClasses, RFcategoricalFeaturesInfo, RFnumTrees, RFfeatureSubsetStrategy, RFimpurity, RFmaxDepth, RFmaxBins)

    // Evaluate model on test instances and compute test error
    labelAndPreds = trainFCNN.map { point =>
      val prediction = modelRF.predict(point.features)
      (point.label, prediction)
    }
    writer.write("RF FCNN filter train TPR x TNR " + tprtnr(labelAndPreds) + "\n")

    labelAndPreds = test.map { point =>
      val prediction = modelRF.predict(point.features)
      (point.label, prediction)
    }
    writer.write("RF FCNN filter test TPR x TNR " + tprtnr(labelAndPreds) + "\n")


    //PCARD
    val cuts = 5
    val trees = 10
    var pcardTrain = PCARD.train(train, trees, cuts)
    
    var pcardPredict = pcardTrain.predict(train)
    var labels = train.map(_.label).collect()
    var cont = 0

    for (i <- labels.indices) {
      if (labels(i) == pcardPredict(i)) {
        cont += 1
      }
    }
    labelAndPreds = sc.parallelize(pcardPredict).zipWithIndex.map
    { case (v, k) => (k, v) }.join(train.zipWithIndex.map
    { case (v, k) => (k, v.label) }).map(_._2)
    writer.write("PCARD train TPR x TNR " + tprtnr(labelAndPreds) + "\n")

    pcardPredict = pcardTrain.predict(test)
    labels = test.map(_.label).collect()
    cont = 0

    for (i <- labels.indices) {
      if (labels(i) == pcardPredict(i)) {
        cont += 1
      }
    }
    labelAndPreds = sc.parallelize(pcardPredict).zipWithIndex.map { case (v, k) => (k, v) }.join(test.zipWithIndex.map { case (v, k) => (k, v.label) }).map(_._2)
    writer.write("PCARD test TPR x TNR " + tprtnr(labelAndPreds) + "\n")

    pcardTrain = PCARD.train(trainROS, trees, cuts)
    
    pcardPredict = pcardTrain.predict(trainROS)
    labels = trainROS.map(_.label).collect()
    cont = 0

    for (i <- labels.indices) {
      if (labels(i) == pcardPredict(i)) {
        cont += 1
      }
    }
    labelAndPreds = sc.parallelize(pcardPredict).zipWithIndex.map { case (v, k) => (k, v) }.join(trainROS.zipWithIndex.map { case (v, k) => (k, v.label) }).map(_._2)
    writer.write("PCARD ROS train TPR x TNR " + tprtnr(labelAndPreds) + "\n")

    pcardPredict = pcardTrain.predict(test)
    labels = test.map(_.label).collect()
    cont = 0

    for (i <- labels.indices) {
      if (labels(i) == pcardPredict(i)) {
        cont += 1
      }
    }
    labelAndPreds = sc.parallelize(pcardPredict).zipWithIndex.map { case (v, k) => (k, v) }.join(test.zipWithIndex.map { case (v, k) => (k, v.label) }).map(_._2)
    writer.write("PCARD ROS test TPR x TNR " + tprtnr(labelAndPreds) + "\n")

    pcardTrain = PCARD.train(trainRUS, trees, cuts)
    
    pcardPredict = pcardTrain.predict(trainRUS)
    labels = trainRUS.map(_.label).collect()
    cont = 0

    for (i <- labels.indices) {
      if (labels(i) == pcardPredict(i)) {
        cont += 1
      }
    }
    labelAndPreds = sc.parallelize(pcardPredict).zipWithIndex.map { case (v, k) => (k, v) }.join(trainRUS.zipWithIndex.map { case (v, k) => (k, v.label) }).map(_._2)
    writer.write("PCARD RUS train TPR x TNR " + tprtnr(labelAndPreds) + "\n")

    pcardPredict = pcardTrain.predict(test)
    labels = test.map(_.label).collect()
    cont = 0

    for (i <- labels.indices) {
      if (labels(i) == pcardPredict(i)) {
        cont += 1
      }
    }
    labelAndPreds = sc.parallelize(pcardPredict).zipWithIndex.map { case (v, k) => (k, v) }.join(test.zipWithIndex.map { case (v, k) => (k, v.label) }).map(_._2)
    writer.write("PCARD ROS test TPR x TNR " + tprtnr(labelAndPreds) + "\n")

    pcardTrain = PCARD.train(trainFCNN, trees, cuts)
    
    pcardPredict = pcardTrain.predict(trainFCNN)
    labels = trainFCNN.map(_.label).collect()
    cont = 0

    for (i <- labels.indices) {
      if (labels(i) == pcardPredict(i)) {
        cont += 1
      }
    }
    labelAndPreds = sc.parallelize(pcardPredict).zipWithIndex.map { case (v, k) => (k, v) }.join(trainFCNN.zipWithIndex.map { case (v, k) => (k, v.label) }).map(_._2)
    writer.write("PCARD FCNN filter train TPR x TNR " + tprtnr(labelAndPreds) + "\n")

    pcardPredict = pcardTrain.predict(test)
    labels = test.map(_.label).collect()
    cont = 0

    for (i <- labels.indices) {
      if (labels(i) == pcardPredict(i)) {
        cont += 1
      }
    }
    labelAndPreds = sc.parallelize(pcardPredict).zipWithIndex.map { case (v, k) => (k, v) }.join(test.zipWithIndex.map { case (v, k) => (k, v.label) }).map(_._2)
    writer.write("PCARD FCNN filter test TPR x TNR " + tprtnr(labelAndPreds) + "\n")

    pcardTrain = PCARD.train(hme_bd_train, trees, cuts)
    pcardPredict = pcardTrain.predict(test)
    labels = test.map(_.label).collect()
    cont = 0

    for (i <- labels.indices) {
      if (labels(i) == pcardPredict(i)) {
        cont += 1
      }
    }
    labelAndPreds = sc.parallelize(pcardPredict).zipWithIndex.map
    { case (v, k) => (k, v) }.join(test.zipWithIndex.map
    { case (v, k) => (k, v.label) }).map(_._2)
    writer.write("PCARD ROS 1 HME-BD TPR x TNR " + tprtnr(labelAndPreds) + "\n")

//Freeing resources
    writer.close()
  }
}