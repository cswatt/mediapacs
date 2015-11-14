from app import app
from flask import render_template

@app.route('/')
@app.route('/index')
def index():
	return render_template('index.html', title='boop')

@app.route('/chart')
def chart():
	return render_template('chart.html', title='hey')