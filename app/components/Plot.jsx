import React from 'react';

export default class Plot extends React.Component {
  constructor(props) {
    super(props);

    // Instance variable to give a unique name for the div to host
    // the Plotly chart.
    this.plotId = 'plot' + props.plot.seq.toString();
  }

  // Initial rendering of Plotly stuff.
  componentDidMount() {
    console.log("mount");
    var data=[{
      values: [19, 26, 77],
      labels: ['beer', 'pizza', 'snacks'],
      type: 'pie'
    }];
    var layout={
      title: 'Just a dummy chart',
      height: 550,
      width: 750
    };
    Plotly.newPlot(this.plotId, data, layout);
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
