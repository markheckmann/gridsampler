/*
*  Set up tour using http://bootstraptour.com/
*/

// extra JS
console.log("tour JS loaded");

// Instance the tour
var tour = new Tour({
  steps: [
  {
    element: "#minimum1",
    title: "Step 1",
    content: "Something goes here..."
  },
  {
    element: "#plot1",
    title: "Step 2",
    content: "Something goes here..."
  }
]});


// Initialize the tour
tour.init();
console.log("tour initialized");

// Start the tour
// tour.start();
