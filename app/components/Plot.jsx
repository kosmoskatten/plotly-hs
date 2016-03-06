import React from 'react';

export default class Plot extends React.Component {
  constructor(props) {
    super(props);
  }

  render() {
    return (
        <div className='w3-card-12 w3-margin' style={{width:800}}>
          <div className='w3-container' style={{height:600}}>
            Hej alla barn.
          </div>
          <div className='w3-container'>
            Bu hu
          </div>
        </div>
    );
  }
}
