{% for entry in journal %}
{{ entry['Date']|format('%F %T(%z) %A') }}
--------------------------

{% if entry.place() %}
{{ entry.place([0, 1]) }}  
{% endif %}
{% if entry.weather('C') %}
{{ entry.weather('C') }}  
{% endif %}
{% if entry['Tags'] %}
Tags: {{ entry['Tags'] }}
{% endif %}

{{ entry['Text'] }}

{% if 'Photo' in entry %}
![Photo for {{entry['Date']|format('%F')}}]({{ entry['Photo'] }})
{% endif %}

{% endfor %}
