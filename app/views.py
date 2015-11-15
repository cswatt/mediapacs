from app import app
from flask import render_template

@app.route('/')
@app.route('/index')
def index():
	return render_template('index.html', title='boop')

@app.route('/chart')
def chart():
	return render_template('chart.html', title='hey')

@app.route('/map1')
def test():
	return render_template('map1.html')

@app.route('/time1')
def time1():
	return render_template('time1.html')