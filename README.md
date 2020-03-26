# Data Mining with R

<!DOCTYPE html>
<html lang="en">
<head>
</head>
<body>
    <div class="jumbotron">
        <div class="col-sm-8 mx-auto">
          <h2>Description</h2>
          <p>The provided R files correspond to solutions for different tasks using data mining techniques, the tasks consist of four topics:</p>         
            <ul>
                <li>Association rules</li>
                <li>Sequential rules</li>
                <li>Classification</li>
                <li>Clustering</li>
            </ul>
            <p>All the files follow a knowledge discovery process consisting of: </p>
            <ul>
                <li>Analysis of the problem</li>
                <li>Data selection</li>
                <li>Data transformation</li>
                <li>Data mining</li>
                <li>Data preparation for evaluation</li>
                <li>Evaluation of results</li>
            </ul>
            <p>
            All the files contain the task, the objectives, the solution step by step with commentaries and conclusions.</p>
            <hr>
          <h2>Content and used methods</h2>
          <h3>- Association rules</h3>
          <ul>
              <li>Apriori, ECLAT - arules package</li>
          </ul>
                    <p>Objective: To find the most interesting association rules in a dataset obtaines from a supermarket (purchase of products and departments)</p>
          <!-- <p></p> -->
          <h3>- Sequential rules</h3>
          <ul>
              <li>cSpade - arulesSequence package</li>
          </ul>
                    <p>Objective: To discover the best sequential rules involving the unusual meal ingestion or unusual exercise activity events.
          The given dataset contains diabetes records from different patients, taken at different time and under certain circumstances </p>
          <h3>- Classification</h3>
          <ul>
              <li>Decision trees: party, rpart - c50 package</li>
              <li>SVM, Naive Bayes - e1071 package</li>
          </ul>
          <p>Objective: To build the best possible classifier for the Age of the Abalone, based on the given dataset and classes: "Young", "Middle" and "Old"</p>
          <p>Every experiment will show their respective confusion matrix and overall accuracy, which will be taken
          into account at the moment of selecting the best classifier for the present problem:
          Classify the Age of the Abalone, using different measurements.</p>
          <!-- </ul> -->
          <h3>- Clustering</h3>
          <ul>
              <li>Partitioning metods: k-means, hclust, cutree</li>
              <li>Cluster, FPC packages</li>
          </ul>
                    <p>Objective: To divide a set of objects into groups including similar objects. The used dataset contains data for red and white wine, the goal is to classify them by their quality using k-means.</p>
        </div>
      </div>
</body>
</html>
