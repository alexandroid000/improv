from flask import Flask, flash, redirect, render_template, request, session, abort
import random

app = Flask(__name__)

@app.route('/')
@app.route('/main.html')
def home_page():
    return render_template('/main.html', main = True)

# @app.route('/cplusplus.html')
# def cpp():
#     return render_template(
#     '/cplusplus.html', cpp=True
#     )


if __name__ == '__main__':
    app.run(host='0.0.0.0')
