properties([
  [
    $class: 'ThrottleJobProperty',
    categories: ['category'],
    limitOneJobWithMatchingParams: false,
    maxConcurrentPerNode: 4,
    maxConcurrentTotal: 0,
    paramsToUseForLimit: '',
    throttleEnabled: true,
    throttleOption: 'category'
  ],
])

pipeline {
    agent none

    stages {
        stage('Prepare') {
            agent {
                label 'linux'
            }
            steps {
                sh 'docker build -t stdcompat . --build-arg UID=$(id -u)'
            }
        }
        stage('Bootstrap') {
            agent {
                label 'linux'
            }
            steps {
                sh 'docker run --rm --volume $PWD:/workspace stdcompat bash -c \'cd /workspace && make -f Makefile.bootstrap\''
                stash name: 'bootstrap'
            }
        }
        stage('Configure') {
            agent {
                label 'linux'
            }
            steps {
                unstash 'bootstrap'
                sh 'docker run --rm --volume $PWD:/workspace stdcompat bash -c \'cd /workspace && mkdir build && cd build && ../configure\''
                stash name: 'configure'
            }
        }
        stage('Build') {
            agent {
                label 'linux'
            }
            steps {
                unstash 'configure'
                sh 'docker run --rm --volume $PWD:/workspace stdcompat bash -c \'cd /workspace/build && make\''
            }
        }
        stage('Tests') {
            agent {
                label 'linux'
            }
            steps {
                script {
                    def switches = sh (
                        script: 'docker run --rm stdcompat opam switch -s',
                        returnStdout: true
                    ).split('\n')
                    def branches = [:]
                    for (i in switches) {
                        def switch_name = i
                        branches["OCaml " + switch_name + " on Linux"] = {
                            node('linux') {
                                sh "rm -rf build"
                                unstash 'bootstrap'
                                sh "docker run --rm --volume \$PWD:/workspace stdcompat sh -c 'cd /workspace && unset BASH_ENV && opam config exec --switch $switch_name -- sh -c '\\''mkdir build && cd build && ../configure && make && make test && ../configure --disable-magic && make && make test'\\'"
                            }
                        }
                    }
                    def versions = ["4.03.0", "4.04.2", "4.05.0", "4.06.1", "4.07.1", "4.08.1", "4.09.1", "4.10.2", "4.11.2", "4.12.1", "4.13.1", "4.14.0"]
                    for (i in versions) {
                        def version = i
                        branches["OCaml " + version + " on Windows"] = {
                            node('windows') {
                                checkout scm
                                bat """
call "C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\VC\\Auxiliary\\Build\\vcvars64.bat"
set PATH=C:\\tools\\cygwin\\bin;%PATH%
bash -c "ci/cygwin-compile-ocaml.sh \\"$version\\""
if not errorlevel 0 exit /b
set PATH=C:\\ocaml\\$version\\bin;%PATH%
set FLEXDIR=C:\\ocaml\\$version\\lib
bash -c "eval \$(~/ocaml-4.09.0/tools/msvs-promote-path) && make -f Makefile.bootstrap && ./configure && make && make test"
                                """
                            }
                        }
                    }
                    throttle(['category']) {
                        parallel branches
                    }
                }
            }
        }
        stage('Deploy') {
            agent {
                label 'linux'
            }
            steps {
                unstash 'configure'
                sh 'docker run --rm --volume $PWD:/workspace stdcompat bash -c \'cd /workspace/build && make dist\''
                archiveArtifacts artifacts: 'build/*.tar.gz', fingerprint: true
            }
        }
    }
}
