import React from 'react';
import JQuery from 'jquery';

const PlotHeight = 550;
const PlotWidth = 750;

export default class Plot extends React.Component {

  constructor(props) {
    super(props);

    // Instance variable to give a unique name for the div to host
    // the Plotly chart.
    this.plotId = 'plot' + props.plot.seq.toString();

    // Instance variable to hold the entry to display.
    this.entry = props.plot.entry;

    // Set the state's plot to an "empty" plot.
    this.state = {
      data: [],
      layout: {
        height: PlotHeight,
        width: PlotWidth,
        title: this.entry.description
      }
    };
  }

  // Initial rendering of Plotly stuff.
  componentDidMount() {
    console.log('mount');
    //var data=[{
    //  values: [19, 26, 77],
    //  labels: ['beer', 'pizza', 'snacks'],
    //  type: 'pie'
    //}];
    //var data=[];
    //var layout={
      //title: 'Just a dummy chart',
      //height: 550,
      //width: 750
    //};
    Plotly.newPlot(this.plotId, this.state.data, this.state.layout);
    console.log('Now fetching from: ' + this.entry.link);
    JQuery.getJSON(this.entry.link, data => {
      console.log("Got somethingi, try update state");
      console.log("h: " + PlotHeight + " w: " + PlotWidth);
      data.layout.height = PlotHeight;
      data.layout.width = PlotWidth;
      data.layout.title = this.entry.description;
      this.setState(data);
    });
  }

  // Update the Plotly stuff with new data and layout.
  componentDidUpdate() {
    console.log("componentDidUpdate()");
    Plotly.newPlot(this.plotId, this.state.data, this.state.layout);
  }

  render() {
    return (
        <div className='w3-card-12 w3-margin' style={{width:800}}>
          <header className='w3-container w3-blue w3-right-align'>
            <span onClick={this.handleClose.bind(this, this.props.plot.seq)}
                  className="clickable">
               ✖
            </span>
          </header>
          <div id={this.plotId}
               className='w3-container' style={{height:600}}>
          </div>
        </div>
    );
  }

  // Event handler when the 'cross' has been clicked.
  handleClose(seq) {
    console.log('handleClose: ' + seq);
    // Propagate the event to the PlotGrid.
    this.props.removePlot(seq);
  }
}
