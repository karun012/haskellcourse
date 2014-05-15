module.exports = function(grunt) {
  grunt.initConfig({
    pkg: grunt.file.readJSON('package.json'),
 
    watch: {
        files: ['*.hs'],
        options: {
            nospawn: true
        }
    },
    shell: {
      docTests: {
         command: function (fileName) {
            return 'doctest ' + fileName;
         },
         stdout: true
        }
    }
  });
  grunt.loadNpmTasks('grunt-contrib-watch');
  grunt.loadNpmTasks('grunt-shell');
  grunt.registerTask('default', ['shell:docTests']);
  grunt.event.on('watch', function(action, filePath){
    grunt.task.run(['shell:docTests:'+filePath]);
  });
};
