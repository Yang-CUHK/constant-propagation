import os
import subprocess

def bfs_traverse(directory):
    queue = [directory]

    while queue:
        current_dir = queue.pop(0)
        for entry in os.listdir(current_dir):
            path = os.path.join(current_dir, entry)
            if os.path.isdir(path):
                queue.append(path)  
            elif entry.endswith('.py'):
                with open(path, 'r') as file:
                    if 'def main' in file.read():
                        print(f"Found: {path}")
                        call_test_script(path)
                        print()
                        print()

def call_test_script(file_path):
    # 构建调用命令
    cmd = [
        "joern",
        "--script", "test1.sc",
        "--param", f"cpgFile={file_path}",
        "--param", "outFile=c.txt"
    ]
    # 执行命令
    subprocess.run(cmd)

if __name__ == "__main__":
    # 获取用户输入的目录路径
    directory_path = input("请输入要遍历的文件夹路径: ")
    bfs_traverse(directory_path)