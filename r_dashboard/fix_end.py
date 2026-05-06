content = open('server.r', encoding='utf-8').read()
# Remove the trailing lone } that was appended
if content.rstrip().endswith('\n}'):
    content = content.rstrip()[:-1].rstrip()
    open('server.r', 'w', encoding='utf-8').write(content + '\n')
    print("Removed trailing }")
else:
    print("Pattern not found, last 50 chars:", repr(content[-50:]))
