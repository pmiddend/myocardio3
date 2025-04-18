function reset_intensity(prefix) {
    const previous_text = document.getElementById(prefix + "-previous").textContent;
    const input_element = document.getElementById(prefix + "-input");
    const position_of_colon = previous_text.indexOf(":");
    if (position_of_colon !== -1) {
	input_element.value = previous_text.substring(0, position_of_colon) + ": ";
    } else {
	if (input_element_value.length > 0) {
	    input_element.value = previous_text + ": ";
	}
    }
}

function decrease_reps(prefix) {
    const input_element = document.getElementById(prefix + "-reps");
    const current_reps = parseInt(input_element.value.trim(), 10);
    const new_reps = current_reps - 1;
    input_element.value = new_reps.toString();
}

function increase_reps(prefix) {
    const input_element = document.getElementById(prefix + "-reps");
    const current_reps = parseInt(input_element.value.trim(), 10);
    const new_reps = current_reps + 1;
    input_element.value = new_reps.toString();
}

function copy_reps(prefix) {
    const reps = document.getElementById(prefix + "-reps").value;

    const current_input = document.getElementById(prefix + "-input").value.trim();
    document.getElementById(prefix + "-input").value = current_input + " " + reps;
}

var timers = [];

const regular_timer = window.setInterval(timer_callback, 500);
function timer_callback() {
    timers.forEach((el) => {
	const seconds_diff = Math.round((new Date().getTime() - el.start_time) / 1000);
	document.getElementById(el.prefix + "-break").value = seconds_diff.toString() + "s";
    });
}

function toggle_timer(prefix) {
    const found_timer = timers.find((el) => el.prefix === prefix);

    if (found_timer === undefined) {
	timers.push({
	    prefix: prefix,
	    start_time: new Date().getTime()
	});

	document.getElementById(prefix + "-break-button").textContent = "⏹️";
	document.getElementById(prefix + "-break").value = "0s";
    } else {
	document.getElementById(prefix + "-break-button").textContent = "▶️";
	timers = timers.filter((el) => el.prefix !== prefix);
	const break_value = document.getElementById(prefix + "-break").value.trim();
	const input_value = document.getElementById(prefix + "-input").value.trim();
	document.getElementById(prefix + "-input").value = input_value + " " + break_value;
    }
}

function write_appendix(prefix) {
    document.getElementById(prefix + "-appendix").insertAdjacentHTML("beforeend", `
	<button class="btn btn-secondary mb-2" type="button" onClick="reset_intensity('${prefix}')">🗄️ Reset intensity</button>

	<div class="input-group mb-3">
	  <input id="${prefix}-reps" class="form-control" type="number" step="1" value="10">
	  <button class="btn btn-outline-secondary" type="button"  style="width: 4em" onClick="decrease_reps('${prefix}')"><strong>➖</strong></button>
	  <button class="btn btn-outline-secondary" type="button" style="width: 4em"onClick="increase_reps('${prefix}')"><strong>➕</strong></button>
	  <button class="btn btn-outline-secondary" type="button" style="width: 4em"onClick="copy_reps('${prefix}')"><strong>📋</strong></button>
	</div>
	<div class="input-group">
	  <input id="${prefix}-break" class="form-control" type="text" value="30s">
	  <button id="${prefix}-break-button" class="btn btn-outline-secondary" style="width: 4em" type="button" onClick="toggle_timer('${prefix}')">▶️</button>
	</div>
            `);
}

