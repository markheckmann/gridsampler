/*
*  Set up tour using http://bootstraptour.com/
*/

// extra JS
console.log("tour JS loaded");

// Instance the tour
var tour = new Tour({
  steps: [

  /*---------- Panel 1 ----------*/
  {
    element: ".well > .container-fluid > div:nth-of-type(1) > h3",
    title: "<b>1. Number of Attributes per Person</b>",
    content: "In this panel, you can define the number of attributes that are elicited per subject. The number is a random variable, following the probability distribution shown in the graphics below.",
    placement: "bottom",
    onShow: function (tour) {
        $("#ourNavbar a[data-value='Simulate']").tab('show');
    }
  },
  {
    element: "#maximum1",
    title: "Range of attributes",
    content: "Allows to define the minimum and maximum number of elicited attributes per subject.<br> <br> <i>Note that maximum cannot be higher than the number of categories in panel 2.</i>",
  },
  {
    element: "#plot1",
    title: "Probability mass distribution",
    content: "Shows the probabilities for the numbers of attributes to be elicited from a single subject.",
  },
  {
    element: ".well > .container-fluid > div:nth-of-type(1) > div:nth-of-type(4)",
    title: "Modify probabilities",
    content: "You can interactively change the probabilities of each number of attributes being elicited."
  },
  {
    element: ".well > .container-fluid > div:nth-of-type(1) > div:nth-of-type(5)",
    title: "Apply probability presets",
    content: "You can insert several types of predefined probability distributions for the random variable <i>number of elicited constructs</i>. A common scenario is a normal-like distribution, with some subjects mentioning more and some less constructs than the average."
  },

 /*---------- Panel 2 ----------*/

  {
    element: ".well > .container-fluid > div:nth-of-type(2) > h3",
    title: "<b>2. Probability of each category</b>",
    content: "Each construct can be assigned to a category. Constructs from some categories are elicited more frequently that from others. A probability distribution for the categories can be assigned. The assumed distribution can be defined and variations in the assumption can be explored.",
    placement: "bottom"
  },
  {
    element: "#maximum2",
    title: "Number of categories",
    content: "Defines the number of categories the constructs stem from. This value usually depends on the field of investigation."
  },
  {
    element: "#plot2",
    title: "Probability mass distribution",
    content: "Shows the probability for eliciting a construct from the certain category.",
  },
  {
    element: ".well > .container-fluid > div:nth-of-type(2) > div:nth-of-type(4)",
    title: "Modify category probabilities",
    content: "You can interactively change the probabilities of a construct from a certain category being elicited."
  },
  {
    element: ".well > .container-fluid > div:nth-of-type(2) > div:nth-of-type(5)",
    title: "Apply probability presets",
    content: "You can insert several types of predefined probability distributions for the probility distribution of the categories. As the order of the categories is arbitrary, it makes sense to order the probabilities in ascending order. Several presets for descending orders are defined.  "
  },

  /*---------- Panel 3 ----------*/

  {
    element: ".well > .container-fluid > div:nth-of-type(3) > h3",
    title: "<b>3. Simulation </b>",
    content: "In panel 3, the simulations takes place, based on the probability distributions in panel 1 and 2. For each subject, the number of elicited constructs is drawn according to the distribution in panel 1. Using this value, a sample is drawn without replacement from the category distribution in panel 2. This process is repeated for all subjects.",
    placement: "bottom"
  },
  {
    element: ".well > .container-fluid > div:nth-of-type(3) > h3",
    title: "<b>3. Simulation - Upper and lower part </b>",
    content: "Panel 3 has an upper and a lower part. In the upper part, a single sample with <i>N</i> subjects is simulated one or many times. In the lower part, the simulation from the upper part is repeated many times across different sample sizes.",
    placement: "bottom"
  },

  /*---------- Panel 3 upper ----------*/

  {
    element: "#sample_random",
    title: "Draw one random sample",
    content: "A random sample of size <i>N</i> as defined in the <i>Sample size</i> input box is drawn according to the probability distributions from panel 1 and 2. The number of constructs obtained for each category is visualized.",
    placement: "left"

  },
  {
    element: "#run_button",
    title: "Draw many random samples",
    content: "Many random samples of size <i>N</i> are drawn. The quantiles of the number of constructs obtained in each category are visualized.",
    placement: "left"
  },

  /*---------- Panel 3 lower ----------*/

  {
    element: "#sample_size2",
    title: "Sample sizes <i>N</i> to simulate",
    content: "Different sample sizes to use in simulation. Seperate the values by comma or define a range using a colon, e.g. <i>10:20</i>. Alternatively, use <i>seq(from, to, by)</i>.",
    placement: "left"
  },
  {
    element: "#runs_per_sample",
    title: "Number of runs",
    content: "Number of samples for each <i>N</i> to be drawn.",
    placement: "left"
  },
  {
    element: "#mincount_m",
    title: "Minimum number of counts per category to display",
    content: "Define the minimal number of counts per category that are visualized. This allows you to steer the level of saturation for the categories.",
    placement: "left"
  },

  {
    element: "#proportion_k",
    title: "Proportions",
    content: "Some categories will contain the minimal number of categories, others will not. Here you can prompt for which proportions of categories that fulfill the minial count (M) you get a visualization of the according probabilities.",
    placement: "left"
  },
    {
    element: "#plot3_2",
    title: "Visualization of probabilities",
    content: "This visualization shows the core results of the simulation. Each panel contains the results for a certain proportion K. The x-axis represents the sample size N. The y-axis shows the probability of getting a proportion of K categories with at least M constructs in it, given the sample size N.",
    placement: "left"
  },

  /*---------- About Tab ----------*/

  {
    element: "#ourNavbar a[data-value='About']",
    title: "More information",
    content: "You will find more thorough information in the about <i>About</i> section.",
    placement: "bottom"
  }

]});


// Initialize the tour
tour.init();
console.log("tour initialized");



// Start the tour
// tour.start();
