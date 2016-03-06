import React from 'react';

export default class Plot extends React.Component {
  constructor(props) {
    super(props);
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
    Plotly.newPlot(this.props.plotId, data, layout);
  }

  render() {
    return (
        <div className='w3-card-12 w3-margin' style={{width:800}}>
          <header className='w3-container w3-blue w3-right-align'>
            <span onClick={this.handleClose.bind(this, this.props.entry)}
                  className="clickable">
               ✖
            </span>
          </header>
          <div id={this.props.plotId}
               className='w3-container' style={{height:600}}>
          </div>
        </div>
    );
  }

  // Event handler when the 'cross' has been clicked.
  handleClose(entry) {
    console.log('handleClose: ' + entry.link);
    // Propagate the event to the PlotGrid.
    this.props.onClose(entry.link);
  }
}
