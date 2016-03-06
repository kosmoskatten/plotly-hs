import React from 'react';

export default class Plot extends React.Component {
  constructor(props) {
    super(props);
  }

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
            ✖
          </header>
          <div id={this.props.plotId}
               className='w3-container' style={{height:600}}>
          </div>
        </div>
    );
  }
}
